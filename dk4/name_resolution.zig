const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const util = @import("util.zig");
const elab = @import("ast_elaboration.zig");

const assert = std.debug.assert;

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

const AST = par.AST;
const AstNode = par.AstNode;
const AstNodeTag = par.AstNodeTag;
const AstNodeIndex = par.AstNodeIndex;

// -----------------------------------------------------------------------
// Public types
// -----------------------------------------------------------------------

pub const DeclIndex = enum(u32) { 
    NONE = 0, 
    // builtin types:
    Int,  
    Float,
    Bool , // Bool must always be the last builtin type for the algo below to work correctly
    _,
};

pub const DeclKind = enum(u8) {
    VARIABLE,
    FN_PARAM,
    FUNCTION,
    STRUCT,
    RESULT_VARIABLE,
    BUILTIN_TYPE,
};

pub const Declaration = struct {
    name_token_idx: TokenIndex = 0,
    kind: DeclKind = .VARIABLE,
    scope_idx: ScopeIndex = .NONE,
    ast_node_idx: AstNodeIndex = 0, // FNDECL, STRUCTDECL, DECLARATION, etc.
};



pub const ScopeKind = enum(u8) { GLOBAL, FILE, FUNCTION, STRUCT_BODY, BLOCK };
pub const ScopeIndex = enum(u32) { NONE = 0, _ };

pub const Scope = struct {
    kind: ScopeKind = .GLOBAL,
    parent: ScopeIndex = .NONE,
};

pub const TypeVarId = u8;

pub const ParamOrMember = struct {
    name_token_idx: TokenIndex = 0,
    type_: union(enum) {
        CONCRETE: DeclIndex, // explicit : Type — known at name resolution
        TYPEVAR: TypeVarId, // bare name, generic — monomorphize per instantiation
        UNRESOLVED: void, // has default, no annotation — type inferred from default during type inference, not generic
    } = .{ .TYPEVAR = 0 },
    default_ast_idx: AstNodeIndex = 0, // 0 = no default

    pub fn isGeneric(self: *const ParamOrMember) bool {
        return self.type_ == .TYPEVAR;
    }
};

pub const ParamsOrMembers = util.ArrayList(ParamOrMember);

pub const FunctionTemplate = struct {
    ast_idx: AstNodeIndex = 0,
    body_ast_idx: AstNodeIndex = 0,
    first_param_idx: ParamsOrMembers.Index = .NONE,
    param_count: u8 = 0,
    typevar_count: u8 = 0,
    decl_idx: DeclIndex = .NONE,

    pub fn params(self: *const FunctionTemplate, params_array: *const ParamsOrMembers) []const ParamOrMember {
        return params_array.slice(self.first_param_idx, self.param_count);
    }
};

pub const FunctionTemplates = util.ArrayList(FunctionTemplate);

pub const StructTemplate = struct {
    ast_idx: AstNodeIndex = 0,
    first_member_idx: ParamsOrMembers.Index = .NONE,
    member_count: u8 = 0,
    typevar_count: u8 = 0, // count of generic members
    decl_idx: DeclIndex = .NONE,

    pub fn members(self: *const StructTemplate, members_array: *const ParamsOrMembers) []const ParamOrMember {
        return members_array.slice(self.first_member_idx, self.member_count);
    }
};

pub const StructTemplates = util.ArrayList(StructTemplate);

/// -----------------------------------------------------------------------
/// DeclInfo — the output of name resolution
/// -----------------------------------------------------------------------
pub const DeclInfo = struct {
    /// All scopes in the program, forming a tree via Scope.parent. Index 0 is null.
    scopes: std.ArrayList(Scope),
    /// All declarations (variables, functions, structs, builtins). Index 0 is null; the next indices thereafter are builtins.
    declarations: std.ArrayList(Declaration),
    /// Maps each AST node (by index) to its resolved declaration. .NONE if not an identifier usage.
    name_resolution: []DeclIndex,

    /// One entry per function definition. Holds param info and body location.
    fn_templates: FunctionTemplates,
    /// One entry per struct definition. Holds member info.
    struct_templates: StructTemplates,
    /// Flat array of fn params and struct members. struct/fn-Templates index into this via first_param/member_idx + count.
    params_or_members: ParamsOrMembers,

    errors: std.ArrayList(Error),
    gpa: std.mem.Allocator,

    pub fn deinit(di: *DeclInfo) void {
        di.scopes.deinit(di.gpa);
        di.declarations.deinit(di.gpa);
        di.gpa.free(di.name_resolution);
        di.fn_templates.deinit();
        di.struct_templates.deinit();
        di.params_or_members.deinit();
        di.errors.deinit(di.gpa);
    }

    pub fn hasErrors(di: *const DeclInfo) bool {
        return di.errors.items.len > 0;
    }

    pub fn declName(di: *const DeclInfo, idx: DeclIndex, ts : *const TokenStream) []const u8 {
        if(@intFromEnum(idx) <= @intFromEnum(DeclIndex.BOOL))
            // Builtin types have hardcoded names
            return @tagName(idx);
        
        const decl = &di.declarations[@intFromEnum(idx)];
        return ts.tokens[decl.name_token_idx].str(ts.source);
    }
};

// -----------------------------------------------------------------------
// Errors
// -----------------------------------------------------------------------

pub const Error = union(enum) {
    undecl_var: TokenIndex,
    undecl_type: TokenIndex,
    multi_decl_var: struct { first_decl: TokenIndex, error_decl: TokenIndex },
    multi_decl_fn: struct { first_decl: TokenIndex, error_decl: TokenIndex },
    decl_shadows_outer: struct { outer_decl: TokenIndex, error_decl: TokenIndex },
    unknown_function: AstNodeIndex,
    symbol_is_not_a_function: struct { declaration: TokenIndex, call: TokenIndex },
    symbol_is_not_a_variable_but_function: struct { declaration: TokenIndex, usage: TokenIndex },
    redeclaration_of_result_var: TokenIndex,
    param_shadows_global: struct { global_decl: TokenIndex, param_decl: TokenIndex },
    function_parameters_have_same_name: struct { first_param: TokenIndex, second_param: TokenIndex },
    duplicate_struct_member: struct { first_decl: TokenIndex, error_decl: TokenIndex },
    struct_name_shadows: struct { first_decl: TokenIndex, error_decl: TokenIndex },
    default_must_be_literal: TokenIndex,
    non_default_param_after_default: struct { param: TokenIndex, first_default: TokenIndex },
    valueless_decl_outside_struct: TokenIndex,
};

// -----------------------------------------------------------------------
// Resolver
// -----------------------------------------------------------------------

const MAX_NESTING_LEVEL = 16;

const SymbolInfo = struct {
    declaration_token: TokenIndex,
    decl_idx: DeclIndex,
    kind: Kind,

    const Kind = enum(u8) { VARIABLE, FN_PARAM, FUNCTION, STRUCT, RESULT_VARIABLE, BUILTIN_TYPE };
};

const SymbolTable = std.hash_map.StringHashMapUnmanaged(SymbolInfo);

const Resolver = struct {
    source: []const u8,
    tokens: []const Token,
    ast: *const AST,

    di: *DeclInfo,
    gpa: std.mem.Allocator,

    // scope stack for walking function bodies
    tables: [MAX_NESTING_LEVEL]SymbolTable = [1]SymbolTable{.{}} ** MAX_NESTING_LEVEL,
    scope_indices: [MAX_NESTING_LEVEL]ScopeIndex = [1]ScopeIndex{.NONE} ** MAX_NESTING_LEVEL,
    nesting_level: usize = 0,

    const ResolverError = error{OutOfMemory};

    fn deinitTables(r: *Resolver) void {
        for (0..MAX_NESTING_LEVEL) |i| {
            r.tables[i].deinit(r.gpa);
        }
    }

    // -- scope helpers --

    fn addScope(r: *Resolver, kind: ScopeKind, parent: ScopeIndex) !ScopeIndex {
        const idx: ScopeIndex = @enumFromInt(r.di.scopes.items.len);
        try r.di.scopes.append(r.gpa, .{ .kind = kind, .parent = parent });
        return idx;
    }

    fn enterBlock(r: *Resolver, kind: ScopeKind) !void {
        const parent = r.scope_indices[r.nesting_level];
        r.nesting_level += 1;
        assert(r.nesting_level < MAX_NESTING_LEVEL);
        r.tables[r.nesting_level].clearRetainingCapacity();
        r.scope_indices[r.nesting_level] = try r.addScope(kind, parent);
    }

    fn exitBlock(r: *Resolver) void {
        assert(r.nesting_level > 0);
        r.tables[r.nesting_level].clearRetainingCapacity();
        r.nesting_level -= 1;
    }

    fn currentScope(r: *const Resolver) ScopeIndex {
        return r.scope_indices[r.nesting_level];
    }

    fn lookupSymbol(r: *const Resolver, name: []const u8) ?SymbolInfo {
        var level: usize = r.nesting_level + 1;
        while (level > 0) {
            level -= 1;
            if (r.tables[level].get(name)) |info|
                return info;
        }
        return null;
    }

    fn putSymbol(r: *Resolver, name: []const u8, info: SymbolInfo) !void {
        try r.tables[r.nesting_level].put(r.gpa, name, info);
    }

    fn addDecl(r: *Resolver, decl: Declaration) !DeclIndex {
        const idx: DeclIndex = @enumFromInt(r.di.declarations.items.len);
        try r.di.declarations.append(r.gpa, decl);
        return idx;
    }

    fn registerBuiltinTypes(r: *Resolver) !void {
        const fields = @typeInfo(DeclIndex).@"enum".fields;
        inline for (fields) |field| {
            if (field.value > @intFromEnum(DeclIndex.NONE) and field.value <= @intFromEnum(DeclIndex.Bool)) {
                const decl_idx = try r.addDecl(.{
                    .name_token_idx = 0,
                    .kind = .BUILTIN_TYPE,
                    .scope_idx = .NONE,
                    .ast_node_idx = 0,
                });
                assert(decl_idx == @as(DeclIndex, @enumFromInt(field.value)));
                try r.putSymbol(field.name, .{
                    .declaration_token = 0,
                    .decl_idx = decl_idx,
                    .kind = .BUILTIN_TYPE,
                });
            }
        }
    }

    fn addError(r: *Resolver, err: Error) !void {
        try r.di.errors.append(r.gpa, err);
    }

    fn tokenStr(r: *const Resolver, idx: TokenIndex) []const u8 {
        return r.tokens[idx].str(r.source);
    }

    // -- Two-pass resolution for declaration scopes --

    /// Two-pass resolution for a declaration scope (file, namespace, struct body with methods).
    /// Pass A: register all FNDECL/STRUCTDECL names.
    /// Pass B: build templates + resolve bodies.
    fn resolveDeclScope(r: *Resolver, block_idx: AstNodeIndex, scope_kind: ScopeKind) !void {
        try r.enterBlock(scope_kind);
        defer r.exitBlock();

        const block = r.ast.get(block_idx);
        assert(block.tag == .BLOCK);

        // --- Pass A: register names ---
        var children_a = block.children(r.ast);
        while (children_a.nextIdx()) |child_idx| {
            const child = r.ast.get(child_idx);
            switch (child.tag) {
                .FNDECL => try r.registerFnOrStruct(child_idx, child, .FUNCTION),
                .STRUCTDECL => try r.registerFnOrStruct(child_idx, child, .STRUCT),
                else => {},
            }
        }

        // --- Pass B: build templates + resolve bodies ---
        var children_b = block.children(r.ast);
        while (children_b.nextIdx()) |child_idx| {
            const child = r.ast.get(child_idx);
            switch (child.tag) {
                .FNDECL => {
                    const name = r.tokenStr(child.token_index);
                    if (r.tables[r.nesting_level].get(name)) |info|
                        try r.resolveFnDecl(child_idx, child, info.decl_idx);
                },
                .STRUCTDECL => {
                    const name = r.tokenStr(child.token_index);
                    if (r.tables[r.nesting_level].get(name)) |info|
                        try r.buildStructTemplate(child_idx, child, info.decl_idx);
                },
                else => unreachable, // declaration scopes only contain fn/struct
            }
        }
    }

    // -- Unified registration --

    fn registerFnOrStruct(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode, kind: DeclKind) !void {
        const name = r.tokenStr(node.token_index);

        // Duplicate check at current scope level
        if (r.tables[r.nesting_level].get(name)) |existing| {
            const err: Error = if (kind == .FUNCTION)
                .{ .multi_decl_fn = .{ .first_decl = existing.declaration_token, .error_decl = node.token_index } }
            else
                .{ .struct_name_shadows = .{ .first_decl = existing.declaration_token, .error_decl = node.token_index } };
            try r.addError(err);
            return;
        }

        // Check outer scopes for shadowing
        if (r.nesting_level > 0) {
            var level: usize = r.nesting_level;
            while (level > 0) {
                level -= 1;
                if (r.tables[level].get(name)) |existing| {
                    try r.addError(.{ .decl_shadows_outer = .{ .outer_decl = existing.declaration_token, .error_decl = node.token_index } });
                    return;
                }
            }
        }

        const decl_idx = try r.addDecl(.{
            .name_token_idx = node.token_index,
            .kind = kind,
            .scope_idx = r.currentScope(),
            .ast_node_idx = node_idx,
        });

        try r.putSymbol(name, .{
            .declaration_token = node.token_index,
            .decl_idx = decl_idx,
            .kind = if (kind == .FUNCTION) .FUNCTION else .STRUCT,
        });
    }

    // -- Resolve function declaration: build template + resolve body --

    fn resolveFnDecl(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode, decl_idx: DeclIndex) !void {
        try r.buildFunctionTemplate(node_idx, node, decl_idx);

        try r.enterBlock(.FUNCTION);

        // Register params
        const ft = r.di.fn_templates.items[r.di.fn_templates.items.len - 1];
        const params = ft.params(&r.di.params_or_members);
        for (params) |param| {
            const param_name = r.tokenStr(param.name_token_idx);
            const param_decl_idx = try r.addDecl(.{
                .name_token_idx = param.name_token_idx,
                .kind = .FN_PARAM,
                .scope_idx = r.currentScope(),
                .ast_node_idx = 0,
            });
            try r.putSymbol(param_name, .{
                .declaration_token = param.name_token_idx,
                .decl_idx = param_decl_idx,
                .kind = .FN_PARAM,
            });
        }

        // Register "result"
        const result_decl_idx = try r.addDecl(.{
            .name_token_idx = node.token_index,
            .kind = .RESULT_VARIABLE,
            .scope_idx = r.currentScope(),
            .ast_node_idx = node_idx,
        });
        try r.putSymbol("result", .{
            .declaration_token = node.token_index,
            .decl_idx = result_decl_idx,
            .kind = .RESULT_VARIABLE,
        });

        // Resolve body
        const fn_params_node = r.ast.get(node.first_child);
        try r.resolveBlock(fn_params_node.next_sibling);

        r.exitBlock();
    }

    // -- Pass 2: build struct templates --

    fn buildStructTemplate(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode, decl_idx: DeclIndex) !void {
        const first_member_idx: ParamsOrMembers.Index = @enumFromInt(r.di.params_or_members.items.len);
        var member_count: u8 = 0;
        var typevar_count: u8 = 0;

        var children = node.children(r.ast);
        while (children.nextIdx()) |member_idx| {
            const member_node = r.ast.get(member_idx);

            // check duplicate member names
            const member_name = r.tokenStr(member_node.token_index);
            const existing_members = r.di.params_or_members.items[@intFromEnum(first_member_idx)..][0..member_count];
            for (existing_members) |em| {
                if (std.mem.eql(u8, r.tokenStr(em.name_token_idx), member_name)) {
                    try r.addError(.{ .duplicate_struct_member = .{ .first_decl = em.name_token_idx, .error_decl = member_node.token_index } });
                    break;
                }
            }

            assert(member_node.tag == .MEMBER);

            const first_child_idx = member_node.first_child;
            if (first_child_idx == 0) {
                _ = try r.di.params_or_members.append(.{
                        .name_token_idx = member_node.token_index,
                        .type_ = .{ .TYPEVAR = typevar_count },
                        .default_ast_idx = 0,
                    });
                typevar_count += 1;
                member_count += 1;
            }
            else {
                // Check children: first child could be TYPE or expr

                const first_child = r.ast.get(first_child_idx);
                if (first_child.tag == .TYPE) {
                    // has type annotation — resolve via symbol table
                    const type_decl = r.resolveTypeAnnotation(first_child.token_index) orelse {
                        try r.addError(.{ .undecl_type = first_child.token_index });
                        member_count += 1;
                        continue;
                    };
                    const default_idx = first_child.next_sibling; // 0 if no default
                    if (default_idx != 0)
                        try r.validateLiteralDefault(default_idx);

                    _ = try r.di.params_or_members.append(.{
                        .name_token_idx = member_node.token_index,
                        .type_ = .{ .CONCRETE = type_decl },
                        .default_ast_idx = default_idx,
                    });
                } else {
                    // no type annotation, has rhs — type inferred from default, not generic
                    const rhs_idx = first_child_idx;
                    try r.validateLiteralDefault(rhs_idx);
                    // TODO: add compile time expression evaluation later.

                    _ = try r.di.params_or_members.append(.{
                        .name_token_idx = member_node.token_index,
                        .type_ = .{ .UNRESOLVED = {} },
                        .default_ast_idx = rhs_idx,
                    });
                }
                member_count += 1;
            }
        }

        _ = try r.di.struct_templates.append(.{
            .ast_idx = node_idx, // node.first_child, // store the STRUCTDECL node_idx.
            .first_member_idx = first_member_idx,
            .member_count = member_count,
            .typevar_count = typevar_count,
            .decl_idx = decl_idx,
        });
    }

    fn validateLiteralDefault(r: *Resolver, expr_idx: AstNodeIndex) !void {
        const expr_node = r.ast.get(expr_idx);
        if (expr_node.tag != .ATOM) {
            try r.addError(.{ .default_must_be_literal = expr_node.token_index });
            return;
        }
        const tag = r.tokens[expr_node.token_index].tag;
        switch (tag) {
            .INT_LIT, .FLOAT_LIT, .TRUE, .FALSE => {},
            else => try r.addError(.{ .default_must_be_literal = expr_node.token_index }),
        }
    }

    // -- Build function templates --

    fn buildFunctionTemplate(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode, decl_idx: DeclIndex) !void {
        const fn_params_node = r.ast.get(node.first_child);
        assert(fn_params_node.tag == .FNPARAMS);

        var child_list = fn_params_node.children(r.ast);
        var param_count: u8 = 0;
        const first_param_idx: ParamsOrMembers.Index = @enumFromInt(r.di.params_or_members.items.len);

        while (child_list.nextIdx()) |_| {
            param_count += 1;
        }

        // Re-iterate to actually register params
        var child_list2 = fn_params_node.children(r.ast);
        var typevar_count: u8 = 0;
        while (child_list2.nextIdx()) |param_idx| {
            const param_node = r.ast.get(param_idx);
            // Check param shadows global
            const param_name = r.tokenStr(param_node.token_index);
            if (r.tables[0].get(param_name)) |existing| {
                try r.addError(.{ .param_shadows_global = .{ .global_decl = existing.declaration_token, .param_decl = param_node.token_index } });
            }

            assert(param_node.tag == .PARAM);

            const first_child_idx = param_node.first_child;
            if (first_child_idx == 0) {
                // bare param (no type, no default) — typevar
                _ = try r.di.params_or_members.append(.{
                    .name_token_idx = param_node.token_index,
                    .type_ = .{ .TYPEVAR = typevar_count },
                });
                typevar_count += 1;
            } else {
                const first_child = r.ast.get(first_child_idx);
                if (first_child.tag == .TYPE) {
                    // has type annotation
                    if (r.resolveTypeAnnotation(first_child.token_index)) |type_decl| {
                        const default_idx = first_child.next_sibling;
                        if (default_idx != 0)
                            try r.validateLiteralDefault(default_idx);
                        _ = try r.di.params_or_members.append(.{
                            .name_token_idx = param_node.token_index,
                            .type_ = .{ .CONCRETE = type_decl },
                            .default_ast_idx = default_idx,
                        });
                    } else {
                        try r.addError(.{ .undecl_type = first_child.token_index });
                        _ = try r.di.params_or_members.append(.{
                            .name_token_idx = param_node.token_index,
                            .type_ = .{ .TYPEVAR = typevar_count },
                        });
                        typevar_count += 1;
                    }
                } else {
                    // no type annotation, has default — UNRESOLVED
                    try r.validateLiteralDefault(first_child_idx);
                    _ = try r.di.params_or_members.append(.{
                        .name_token_idx = param_node.token_index,
                        .type_ = .{ .UNRESOLVED = {} },
                        .default_ast_idx = first_child_idx,
                    });
                }
            }
        }

        // Check duplicate param names
        for (0..param_count) |i| {
            for (i + 1..param_count) |j| {
                const ti_idx = r.di.params_or_members.items[@intFromEnum(first_param_idx) + i].name_token_idx;
                const tj_idx = r.di.params_or_members.items[@intFromEnum(first_param_idx) + j].name_token_idx;
                if (std.mem.eql(u8, r.tokenStr(ti_idx), r.tokenStr(tj_idx)))
                    try r.addError(.{ .function_parameters_have_same_name = .{ .first_param = ti_idx, .second_param = tj_idx } });
            }
        }

        // Check defaults must be trailing
        const params_slice = r.di.params_or_members.items[@intFromEnum(first_param_idx)..][0..param_count];
        var first_default_token: ?TokenIndex = null;
        for (params_slice) |p| {
            if (p.default_ast_idx != 0) {
                if (first_default_token == null)
                    first_default_token = p.name_token_idx;
            } else if (first_default_token) |fd| {
                try r.addError(.{ .non_default_param_after_default = .{ .param = p.name_token_idx, .first_default = fd } });
            }
        }

        const body_ast_idx = fn_params_node.next_sibling;
        assert(body_ast_idx > 0);

        _ = try r.di.fn_templates.append(.{
            .ast_idx = node_idx,
            .body_ast_idx = body_ast_idx,
            .param_count = param_count,
            .typevar_count = typevar_count,
            .first_param_idx = first_param_idx,
            .decl_idx = decl_idx,
        });
    }

    // -- Resolve blocks and statements --

    fn resolveBlock(r: *Resolver, block_idx: AstNodeIndex) !void {
        const block = r.ast.get(block_idx);
        assert(block.tag == .BLOCK);

        try r.enterBlock(.BLOCK);
        defer r.exitBlock();

        var children = block.children(r.ast);
        while (children.nextIdx()) |child_idx|
            try r.resolveStatement(child_idx);
    }

    fn resolveStatement(r: *Resolver, stmt_idx: AstNodeIndex) error{OutOfMemory}!void {
        const node = r.ast.get(stmt_idx);
        switch (node.tag) {
            .DECLARATION => try r.resolveDeclaration(stmt_idx, node),
            .ASSIGNMENT => try r.resolveAssignment(stmt_idx, node),
            .CALL_OR_INST => try r.resolveCallOrInst(stmt_idx),
            .IF => try r.resolveIf(stmt_idx, node),
            .WHILE => try r.resolveWhile(stmt_idx, node),
            .BLOCK => try r.resolveBlock(stmt_idx),
            .FNDECL => {
                try r.registerFnOrStruct(stmt_idx, node, .FUNCTION);
                const name = r.tokenStr(node.token_index);
                if (r.tables[r.nesting_level].get(name)) |info|
                    try r.resolveFnDecl(stmt_idx, node, info.decl_idx);
            },
            .STRUCTDECL => {
                try r.registerFnOrStruct(stmt_idx, node, .STRUCT);
                const name = r.tokenStr(node.token_index);
                if (r.tables[r.nesting_level].get(name)) |info|
                    try r.buildStructTemplate(stmt_idx, node, info.decl_idx);
            },
            .RETURN => {
                if (node.first_child != 0)
                    try r.resolveExpr(node.first_child);
            },
            else => {},
        }
    }

    fn resolveDeclaration(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode) !void {
        // token_index = declared IDENT
        // children: [optional TYPE, optional rhs_expr]
        const var_name = r.tokenStr(node.token_index);

        // Resolve RHS first (before registering the variable, so x := x is an error)
        const first_child_idx = node.first_child;
        var rhs_idx: AstNodeIndex = 0;

        if (first_child_idx != 0) {
            const first_child = r.ast.get(first_child_idx);
            if (first_child.tag == .TYPE) {
                // type annotation — resolve type name if it refers to a struct
                try r.resolveTypeName(first_child_idx, first_child);
                rhs_idx = first_child.next_sibling;
            } else {
                rhs_idx = first_child_idx;
            }
        }

        if (rhs_idx == 0 and first_child_idx != 0) {
            // `x : Type` with no value — only valid inside struct body
            // Check we are not inside a struct body (struct members are handled separately)
            // This path is reached for `x : Int` inside a function body
            const first_child = r.ast.get(first_child_idx);
            if (first_child.tag == .TYPE) {
                try r.addError(.{ .valueless_decl_outside_struct = node.token_index });
                return;
            }
        }

        if (rhs_idx != 0)
            try r.resolveExpr(rhs_idx);

        // Register variable
        try r.registerVariable(node.token_index, var_name, node_idx);
    }

    fn resolveAssignment(r: *Resolver, _: AstNodeIndex, node: *AstNode) !void {
        // children: lhs (ATOM or MEMBER_ACCESS), rhs
        const lhs_idx = node.first_child;
        assert(lhs_idx != 0);
        const lhs = r.ast.get(lhs_idx);
        const rhs_idx = lhs.next_sibling;
        assert(rhs_idx != 0);

        // Resolve LHS
        if (lhs.tag == .ATOM)
            try r.resolveAtomAsLValue(lhs_idx, lhs)
        else if (lhs.tag == .MEMBER_ACCESS)
            try r.resolveMemberAccessBase(lhs_idx)
        else if (lhs.tag == .ARRAY_ACCESS)
            try r.resolveArrayAccess(lhs_idx)
        else
            unreachable;

        // Resolve RHS
        try r.resolveExpr(rhs_idx);
    }

    fn resolveAtomAsLValue(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode) !void {
        const token = r.tokens[node.token_index];
        if (token.tag != .IDENTIFIER) return;

        const name = r.tokenStr(node.token_index);
        if (r.lookupSymbol(name)) |info| {
            r.di.name_resolution[node_idx] = info.decl_idx;
            switch (info.kind) {
                .FUNCTION, .STRUCT, .BUILTIN_TYPE => try r.addError(.{ .symbol_is_not_a_variable_but_function = .{ .declaration = info.declaration_token, .usage = node.token_index } }),
                else => {},
            }
        } else {
            try r.addError(.{ .undecl_var = node.token_index });
        }
    }

    fn resolveCallOrInst(r: *Resolver, node_idx: AstNodeIndex) !void {
        const node = r.ast.get(node_idx);
        assert(node.tag == .CALL_OR_INST);
        const callee_name = r.tokenStr(node.token_index);

        // Resolve callee
        if (r.lookupSymbol(callee_name)) |info| {
            r.di.name_resolution[node_idx] = info.decl_idx;
            switch (info.kind) {
                .FUNCTION, .STRUCT => {}, // valid
                else => try r.addError(.{ .symbol_is_not_a_function = .{ .declaration = info.declaration_token, .call = node.token_index } }),
            }
        } else {
            try r.addError(.{ .unknown_function = node_idx });
        }

        // Resolve arguments
        var children = node.children(r.ast);
        while (children.nextIdx()) |arg_idx| {
            const arg = r.ast.get(arg_idx);
            if (arg.tag == .NAMED_ARG) {
                // resolve the value expression (first_child), not the name
                if (arg.first_child != 0)
                    try r.resolveExpr(arg.first_child);
            } else {
                try r.resolveExpr(arg_idx);
            }
        }
    }

    fn resolveIf(r: *Resolver, _: AstNodeIndex, node: *AstNode) !void {
        const cte = node.conditionThenElse(r.ast);
        try r.resolveExpr(cte.cond_idx);
        try r.resolveStatement(cte.then_idx);
        if (cte.else_idx != 0)
            try r.resolveStatement(cte.else_idx);
    }

    fn resolveWhile(r: *Resolver, _: AstNodeIndex, node: *AstNode) !void {
        const cte = node.conditionThenElse(r.ast);
        try r.resolveExpr(cte.cond_idx);
        try r.resolveStatement(cte.then_idx);
    }

    fn resolveExpr(r: *Resolver, expr_idx: AstNodeIndex) error{OutOfMemory}!void {
        const node = r.ast.get(expr_idx);
        switch (node.tag) {
            .ATOM => try r.resolveAtom(expr_idx, node),
            .BINARY_OP => {
                try r.resolveExpr(node.first_child);
                try r.resolveExpr(r.ast.get(node.first_child).next_sibling);
            },
            .UNARY_OP => try r.resolveExpr(node.first_child),
            .CALL_OR_INST => try r.resolveCallOrInst(expr_idx),
            .MEMBER_ACCESS => try r.resolveMemberAccessBase(expr_idx),
            .ARRAY_ACCESS => try r.resolveArrayAccess(expr_idx),
            .ARRAY_LIT => try r.resolveArrayLit(expr_idx),
            .FILL => try r.resolveExpr(node.first_child),
            else => {},
        }
    }

    fn resolveArrayAccess(r: *Resolver, node_idx: AstNodeIndex) !void {
        // first_child = base expr, base.next_sibling = INDEX_ARGS
        const node = r.ast.get(node_idx);
        try r.resolveExpr(node.first_child);
        const index_args = r.ast.get(node.first_child).next_sibling;
        var idx = r.ast.get(index_args).first_child;
        while (idx != 0) : (idx = r.ast.get(idx).next_sibling) {
            try r.resolveExpr(idx);
        }
    }

    fn resolveArrayLit(r: *Resolver, node_idx: AstNodeIndex) !void {
        var child = r.ast.get(node_idx).first_child;
        while (child != 0) : (child = r.ast.get(child).next_sibling) {
            try r.resolveExpr(child);
        }
    }

    fn resolveAtom(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode) !void {
        const token = r.tokens[node.token_index];
        if (token.tag != .IDENTIFIER) return; // literal

        const name = r.tokenStr(node.token_index);
        if (r.lookupSymbol(name)) |info| {
            r.di.name_resolution[node_idx] = info.decl_idx;
            switch (info.kind) {
                .FUNCTION, .STRUCT, .BUILTIN_TYPE => try r.addError(.{ .symbol_is_not_a_variable_but_function = .{ .declaration = info.declaration_token, .usage = node.token_index } }),
                else => {},
            }
        } else {
            try r.addError(.{ .undecl_var = node.token_index });
        }
    }

    fn resolveMemberAccessBase(r: *Resolver, node_idx: AstNodeIndex) !void {
        // MEMBER_ACCESS: token_index = field name, first_child = base expression
        // Only resolve the base expression. Field name requires types (done later).
        const node = r.ast.get(node_idx);
        assert(node.tag == .MEMBER_ACCESS);
        try r.resolveExpr(node.first_child);
    }

    fn resolveTypeName(r: *Resolver, node_idx: AstNodeIndex, node: *AstNode) !void {
        // TYPE node — resolve it to a declaration (builtin or user-defined)
        const name = r.tokenStr(node.token_index);
        if (r.lookupSymbol(name)) |info|
            r.di.name_resolution[node_idx] = info.decl_idx;
    }

    /// Look up a type name in the symbol table, return its DeclIndex or null.
    fn resolveTypeAnnotation(r: *const Resolver, token_idx: TokenIndex) ?DeclIndex {
        const name = r.tokenStr(token_idx);
        if (r.lookupSymbol(name)) |info|
            return info.decl_idx;
        return null;
    }

    fn registerVariable(r: *Resolver, token_idx: TokenIndex, name: []const u8, ast_node_idx: AstNodeIndex) !void {
        // Check shadows in outer scopes
        var level: usize = r.nesting_level;
        while (level > 0) {
            level -= 1;
            if (r.tables[level].get(name)) |existing| {
                if (existing.kind == .RESULT_VARIABLE) {
                    try r.addError(.{ .redeclaration_of_result_var = token_idx });
                } else {
                    try r.addError(.{ .decl_shadows_outer = .{ .outer_decl = existing.declaration_token, .error_decl = token_idx } });
                }
                return; // don't register
            }
        }

        // Check current scope
        if (r.tables[r.nesting_level].get(name)) |existing| {
            if (existing.kind == .RESULT_VARIABLE) {
                try r.addError(.{ .redeclaration_of_result_var = token_idx });
            } else {
                try r.addError(.{ .multi_decl_var = .{ .first_decl = existing.declaration_token, .error_decl = token_idx } });
            }
            return;
        }

        const decl_idx = try r.addDecl(.{
            .name_token_idx = token_idx,
            .kind = .VARIABLE,
            .scope_idx = r.currentScope(),
            .ast_node_idx = ast_node_idx,
        });
        r.di.name_resolution[ast_node_idx] = decl_idx;
        try r.putSymbol(name, .{
            .declaration_token = token_idx,
            .decl_idx = decl_idx,
            .kind = .VARIABLE,
        });
    }



};

// -----------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------

pub fn resolve(
    gpa: std.mem.Allocator,
    ast: *const AST,
    tokens: []const Token,
    source: []const u8,
    root_idx: AstNodeIndex,
) !DeclInfo {
    const name_res = try gpa.alloc(DeclIndex, ast.nodeCount());
    @memset(name_res, .NONE);

    var di = DeclInfo{
        .scopes = try .initCapacity(gpa, 16),
        .declarations = try .initCapacity(gpa, 32),
        .name_resolution = name_res,
        .fn_templates = try FunctionTemplates.init(gpa, 16),
        .struct_templates = try StructTemplates.init(gpa, 8),
        .params_or_members = try ParamsOrMembers.init(gpa, 48),
        .errors = try .initCapacity(gpa, 4),
        .gpa = gpa,
    };

    // Add null scope and null declaration at index 0
    try di.scopes.append(gpa, .{});
    try di.declarations.append(gpa, .{});

    var resolver = Resolver{
        .source = source,
        .tokens = tokens,
        .ast = ast,
        .di = &di,
        .gpa = gpa,
    };
    defer resolver.deinitTables();

    // Level 0: GLOBAL scope with builtins
    resolver.scope_indices[0] = try resolver.addScope(.GLOBAL, .NONE);
    try resolver.registerBuiltinTypes();

    // Level 1: FILE scope — two-pass resolution of top-level declarations
    try resolver.resolveDeclScope(root_idx, .FILE);

    return di;
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

test "resolve simple program" {
    const gpa = std.testing.allocator;
    const source =
        \\fn main()
        \\    x := 42
        \\    result = x
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(!di.hasErrors());
    // Should have 1 function template (main) at index 1
    try std.testing.expectEqual(@as(usize, 2), di.fn_templates.items.len); // [0]=null, [1]=main
}

test "resolve undeclared variable" {
    const gpa = std.testing.allocator;
    const source =
        \\fn main()
        \\    result = y
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(di.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), di.errors.items.len);
    try std.testing.expect(di.errors.items[0] == .undecl_var);
}

test "resolve duplicate variable" {
    const gpa = std.testing.allocator;
    const source =
        \\fn main()
        \\    x := 1
        \\    x := 2
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(di.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), di.errors.items.len);
    try std.testing.expect(di.errors.items[0] == .multi_decl_var);
}

test "resolve function call" {
    const gpa = std.testing.allocator;
    const source =
        \\fn add(a, b)
        \\    result = a + b
        \\
        \\fn main()
        \\    x := add(1, 2)
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(!di.hasErrors());
    try std.testing.expectEqual(@as(usize, 3), di.fn_templates.items.len); // [0]=null, [1]=add, [2]=main
}

test "resolve struct declaration" {
    const gpa = std.testing.allocator;
    const source =
        \\struct Car
        \\    velocity := 0.0
        \\    gear : Int
        \\
        \\fn main()
        \\    result = 0
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(!di.hasErrors());
    try std.testing.expectEqual(@as(usize, 2), di.struct_templates.items.len); // [0]=null, [1]=Car
    const car = di.struct_templates.items[1];
    try std.testing.expectEqual(@as(u8, 2), car.member_count);
    try std.testing.expectEqual(@as(u8, 0), car.typevar_count); // velocity is UNRESOLVED (not generic), gear is CONCRETE
}

test "resolve struct with generic member" {
    const gpa = std.testing.allocator;
    const source =
        \\struct Pair
        \\    first
        \\    second
        \\
        \\fn main()
        \\    result = 0
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(!di.hasErrors());
    const pair = di.struct_templates.items[1];
    try std.testing.expectEqual(@as(u8, 2), pair.member_count);
    try std.testing.expectEqual(@as(u8, 2), pair.typevar_count);
}

test "resolve struct instantiation" {
    const gpa = std.testing.allocator;
    const source =
        \\struct Car
        \\    velocity := 0.0
        \\    gear : Int = 1
        \\
        \\fn main()
        \\    c := Car(gear=3)
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(!di.hasErrors());
    // The CALL_OR_INST for Car should resolve to the struct declaration
    var found_car_call = false;
    for (di.name_resolution, 0..) |decl_idx, ast_idx| {
        if (decl_idx != .NONE) {
            const decl = di.declarations.items[@intFromEnum(decl_idx)];
            if (decl.kind == .STRUCT) {
                const call_node = pr.ast.get(@intCast(ast_idx));
                if (call_node.tag == .CALL_OR_INST)
                    found_car_call = true;
            }
        }
    }
    try std.testing.expect(found_car_call);
}

test "resolve duplicate struct member" {
    const gpa = std.testing.allocator;
    const source =
        \\struct Bad
        \\    x := 1
        \\    x := 2
        \\
        \\fn main()
        \\    result = 0
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(di.hasErrors());
    try std.testing.expect(di.errors.items[0] == .duplicate_struct_member);
}

test "resolve shadowing error" {
    const gpa = std.testing.allocator;
    const source =
        \\fn foo(x)
        \\    result = x
        \\
        \\fn main()
        \\    x := 1
        \\    if x > 0
        \\        x := 2
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(di.hasErrors());
    try std.testing.expect(di.errors.items[0] == .decl_shadows_outer);
}

test "resolve unknown function" {
    const gpa = std.testing.allocator;
    const source =
        \\fn main()
        \\    x := unknown(1)
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(di.hasErrors());
    try std.testing.expect(di.errors.items[0] == .unknown_function);
}

test "resolve member access" {
    const gpa = std.testing.allocator;
    const source =
        \\struct Car
        \\    velocity := 0.0
        \\
        \\fn main()
        \\    c := Car()
        \\    result = c.velocity
    ;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    // Member access field resolution is deferred to type inference,
    // so no error here (only base object 'c' is resolved).
    try std.testing.expect(!di.hasErrors());
}
