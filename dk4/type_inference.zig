const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const util = @import("util.zig");
const ft_ast = @import("ft_ast.zig");
const nr = @import("name_resolution.zig");

const assert = std.debug.assert;

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;
const SourceLoc = tok.SourceLoc;

const AST = par.AST;
const AstNode = par.AstNode;
const AstNodeIndex = par.AstNodeIndex;

const FtAst = ft_ast.FtAst;
const DkType = ft_ast.DkType;
const TypeError = ft_ast.TypeError;
const TypeErrorInfo = ft_ast.TypeErrorInfo;
const BinaryOp = ft_ast.BinaryOp;
const UnaryOp = ft_ast.UnaryOp;
const AssignmentKind = ft_ast.AssignmentKind;
const ScopeIndex = ft_ast.ScopeIndex;
const StamentIndex = ft_ast.StamentIndex;
const ExpressionIndex = ft_ast.ExpressionIndex;
const VarDeclIndex = ft_ast.VarDeclIndex;
const FnDeclIndex = ft_ast.FnDeclIndex;
const StructInstanceIndex = ft_ast.StructInstanceIndex;

const DeclIndex = nr.DeclIndex;
const DeclInfo = nr.DeclInfo;
const ParamOrMember = nr.ParamOrMember;
const ParamsOrMembers = nr.ParamsOrMembers;

const MAX_NUM_FUNCTION_PARAMS = 32;
const MAX_TYPEVARS = 32;

// -----------------------------------------------------------------------
// DkType ↔ DeclIndex mapping for builtin types
// -----------------------------------------------------------------------

fn dkTypeFromBuiltinDecl(decl: DeclIndex) ?DkType {
    return switch (decl) {
        .Int => DkType.INT,
        .Float => DkType.FLOAT,
        .Bool => DkType.BOOL,
        else => null,
    };
}

// -----------------------------------------------------------------------
// Function Instances
// -----------------------------------------------------------------------

pub const FunctionInstance = struct {
    fn_template_idx: nr.FunctionTemplates.Index,
    return_type: DkType = DkType.UNKNOWN,
    param_count: u8 = 0,
    first_param_type_idx: Types.Index = .NONE,
    fn_decl_idx: FnDeclIndex = 0, // index into FtAst.fn_decls
};

pub const FunctionInstanceKey = struct {
    template_idx: nr.FunctionTemplates.Index,
    param_types: Types.IndexRange,
};
pub const FunctionInstancePseudoKey = struct {
    template_idx: nr.FunctionTemplates.Index,
    param_types: []const DkType,
};

pub const Types = util.ArrayList(DkType);

pub const FunctionInstanceHashContext = struct {
    types: *const Types,

    fn combineHash(h: u64, t: DkType) u64 {
        return h ^ (@as(u64, @as(u32, @bitCast(t))) +% 0x9e3779b9 +% (h << 6) +% (h >> 2));
    }

    fn hashKey(ctx: FunctionInstanceHashContext, key: FunctionInstanceKey) u64 {
        var h: u64 = @intFromEnum(key.template_idx);
        var it = key.param_types.iterator();
        while (it.next()) |i|
            h = combineHash(h, ctx.types.get(i));
        return h;
    }

    fn hashPseudoKey(_: FunctionInstanceHashContext, key: FunctionInstancePseudoKey) u64 {
        var h: u64 = @intFromEnum(key.template_idx);
        for (key.param_types) |t|
            h = combineHash(h, t);
        return h;
    }

    pub fn hash(ctx: FunctionInstanceHashContext, key: anytype) u32 {
        return @intCast(0xffffffff & switch (@TypeOf(key)) {
            FunctionInstanceKey => ctx.hashKey(key),
            FunctionInstancePseudoKey => ctx.hashPseudoKey(key),
            else => unreachable,
        });
    }

    pub fn eql(ctx: FunctionInstanceHashContext, a: anytype, b: FunctionInstanceKey, index: usize) bool {
        _ = index;
        switch (@TypeOf(a)) {
            FunctionInstanceKey => return ctx.eqlKeyKey(a, b),
            FunctionInstancePseudoKey => return ctx.eqlPseudoKey(a, b),
            else => unreachable,
        }
    }

    fn eqlPseudoKey(ctx: FunctionInstanceHashContext, a: FunctionInstancePseudoKey, b: FunctionInstanceKey) bool {
        if (a.template_idx != b.template_idx) return false;
        if (a.param_types.len != b.param_types.len()) return false;
        for (a.param_types, ctx.types.sliceFromRange(b.param_types)) |at, bt| {
            if (at != bt) return false;
        }
        return true;
    }

    fn eqlKeyKey(ctx: FunctionInstanceHashContext, a: FunctionInstanceKey, b: FunctionInstanceKey) bool {
        if (a.template_idx != b.template_idx) return false;
        if (a.param_types.len() != b.param_types.len()) return false;
        for (ctx.types.sliceFromRange(a.param_types), ctx.types.sliceFromRange(b.param_types)) |at, bt| {
            if (at != bt) return false;
        }
        return true;
    }
};

pub const FunctionInstances = struct {
    hash_map: HashMap,
    gpa: std.mem.Allocator,
    param_types: Types,

    pub const HashMap = std.ArrayHashMapUnmanaged(FunctionInstanceKey, FunctionInstance, FunctionInstanceHashContext, true);
    pub const Index = usize;

    const CTX = FunctionInstanceHashContext;

    pub fn init(gpa: std.mem.Allocator) !FunctionInstances {
        return .{
            .hash_map = .empty,
            .gpa = gpa,
            .param_types = try .initWithNullElement(gpa, 128, DkType.UNKNOWN),
        };
    }

    pub fn deinit(self: *FunctionInstances) void {
        self.hash_map.deinit(self.gpa);
        self.param_types.deinit();
    }

    pub fn getPtr(self: *FunctionInstances, key: FunctionInstancePseudoKey) ?*FunctionInstance {
        return self.hash_map.getPtrAdapted(key, CTX{ .types = &self.param_types });
    }

    pub fn contains(self: *FunctionInstances, key: FunctionInstancePseudoKey) bool {
        return self.hash_map.containsAdapted(key, CTX{ .types = &self.param_types });
    }

    pub fn put(self: *FunctionInstances, key: FunctionInstanceKey, value: FunctionInstance) !HashMap.GetOrPutResult {
        const res = try self.hash_map.getOrPutContextAdapted(self.gpa, key, CTX{ .types = &self.param_types }, CTX{ .types = &self.param_types });
        assert(!res.found_existing);
        res.key_ptr.* = key;
        res.value_ptr.* = value;
        return res;
    }

    pub fn getByIndex(self: *const FunctionInstances, index: Index) FunctionInstance {
        return self.hash_map.entries.get(index).value;
    }
};

// -----------------------------------------------------------------------
// Struct Instances
// -----------------------------------------------------------------------

pub const StructInstance = struct {
    template_idx: nr.StructTemplates.Index,
    member_count: u8 = 0,
    first_member_type_idx: Types.Index = .NONE,
};

pub fn infer(
    gpa: std.mem.Allocator,
    ts: TokenStream,
    ast: *const AST,
    di: *const DeclInfo,
) !FtAst {
    var ft = try FtAst.init(gpa);
    errdefer ft.deinit();

    var ti = TypeInferer{
        .source = ts.source,
        .tokens = ts.tokens,
        .ts = ts,
        .ast = ast,
        .di = di,
        .ft = &ft,
        .fn_instances = try .init(gpa),
        .struct_instances = try .initCapacity(gpa, 16),
        .types = try .initWithNullElement(gpa, 128, DkType.UNKNOWN),
        .decl_map = .empty,
        .current_scope = 0,
        .fn_instantiation_stack = try .initCapacity(gpa, 64),
        .fn_instantiation_ast_idx_stack = try .initCapacity(gpa, 64),
        .gpa = gpa,
    };
    defer ti.deinit();

    try ti.checkAndReconstructTypes();
    return ft;
}

// -----------------------------------------------------------------------
// Array shape inference helpers (free functions, no TypeInferer needed)
// -----------------------------------------------------------------------

const DimInfo = struct {
    count: u16,
    has_fill: bool,
};

const ArrayShape = struct {
    dims: [ft_ast.MAX_ARRAY_NDIM]DimInfo,
    ndim: u8,
    extra_fill_node: AstNodeIndex, // 0 = ok; non-zero = second FILL found (node idx of offender)
};

fn arrayShapeFromLit(ast: *const AST, lit_node_idx: AstNodeIndex) ArrayShape {
    const lit_node = ast.get(lit_node_idx);
    var result = ArrayShape{
        .dims = undefined,
        .ndim = 1,
        .extra_fill_node = 0,
    };
    result.dims[0] = .{ .count = 0, .has_fill = false };
    var inner: ?ArrayShape = null;
    var fill_seen = false;

    var child_iter = lit_node.children(ast);
    while (child_iter.nextIdx()) |child_idx| {
        const child = ast.get(child_idx);
        if (child.tag == .FILL) {
            if (!fill_seen) {
                fill_seen = true;
                result.dims[0].has_fill = true;
                if (child.first_child != 0) {
                    const fill_val = ast.get(child.first_child);
                    if (fill_val.tag == .ARRAY_LIT and inner == null)
                        inner = arrayShapeFromLit(ast, child.first_child);
                }
            } else {
                result.extra_fill_node = child_idx;
            }
        } else if (child.tag == .ARRAY_LIT) {
            result.dims[0].count += 1;
            if (inner == null)
                inner = arrayShapeFromLit(ast, child_idx);
        } else {
            result.dims[0].count += 1;
        }
    }

    if (inner) |inner_shape| {
        result.ndim = 1 + inner_shape.ndim;
        for (0..inner_shape.ndim) |i|
            result.dims[1 + i] = inner_shape.dims[i];
        if (inner_shape.extra_fill_node != 0 and result.extra_fill_node == 0)
            result.extra_fill_node = inner_shape.extra_fill_node;
    }

    return result;
}

// -----------------------------------------------------------------------
// TypeInferer
// -----------------------------------------------------------------------

const TypeInferer = struct {
    // From parser
    source: []const u8,
    tokens: []const Token,
    ts: TokenStream,
    ast: *const AST,

    // From name resolution
    di: *const DeclInfo,

    // Output: FtAst being built
    ft: *FtAst,

    // Instantiation tracking
    fn_instances: FunctionInstances,
    struct_instances: std.ArrayList(StructInstance),
    types: Types, // shared type array for struct member types (fn param types use fn_instances.param_types)

    // Per-instantiation state
    decl_map: std.AutoHashMapUnmanaged(DeclIndex, VarDeclIndex),
    current_scope: ScopeIndex,

    // Error tracking
    fn_instantiation_stack: std.ArrayList(FunctionInstances.Index),
    fn_instantiation_ast_idx_stack: std.ArrayList(AstNodeIndex),

    gpa: std.mem.Allocator,

    const Error = TypeError;
    const ErrorInfo = TypeErrorInfo;

    const TypeInfererException = error{
        OutOfMemory,
        WrongNumberOfFunctionArguments,
    };

    fn deinit(ti: *TypeInferer) void {
        ti.fn_instances.deinit();
        ti.struct_instances.deinit(ti.gpa);
        ti.types.deinit();
        ti.decl_map.deinit(ti.gpa);
        ti.fn_instantiation_stack.deinit(ti.gpa);
        ti.fn_instantiation_ast_idx_stack.deinit(ti.gpa);
    }

    // -----------------------------------------------------------------------
    // Error helpers
    // -----------------------------------------------------------------------

    fn addError(ti: *TypeInferer, error_: Error) !void {
        const len = @min(ti.fn_instantiation_stack.items.len, ErrorInfo.MAX_FN_INSTANTIATION_REPORT_DEPTH);
        var error_info = ErrorInfo{
            .error_ = error_,
            .fn_instantiation_stack_top = undefined,
            .fn_instantiation_stack_len = len,
        };
        for (0..len) |i| {
            const idx1 = @as(i32, @intCast(ti.fn_instantiation_stack.items.len)) - @as(i32, @intCast(1 + i));
            const idx2 = @as(i32, @intCast(ti.fn_instantiation_ast_idx_stack.items.len)) - @as(i32, @intCast(i));
            error_info.fn_instantiation_stack_top[i] = .{
                .function = ti.fn_instantiation_stack.items[@intCast(idx1)],
                .fn_call_ast_idx = if (idx2 >= 0 and idx2 < ti.fn_instantiation_ast_idx_stack.items.len) ti.fn_instantiation_ast_idx_stack.items[@intCast(idx2)] else 0,
            };
        }
        try ti.ft.errors.append(ti.gpa, error_info);
    }

    fn expectType(ti: *TypeInferer, node_idx: AstNodeIndex, actual: DkType, comptime expected: DkType) !void {
        if (actual == DkType.ERROR) return;
        if (actual != expected)
            try ti.addError(.{ .wrong_type = .{ .ast_node = node_idx, .actual = actual, .expected = comptime expected.langName() } });
    }

    // -----------------------------------------------------------------------
    // Main entry point
    // -----------------------------------------------------------------------

    fn checkAndReconstructTypes(ti: *TypeInferer) !void {
        // Instantiate all non-generic functions
        for (ti.di.fn_templates.items[1..], 1..) |ft_item, i| {
            if (ft_item.typevar_count == 0) {
                const ft_idx: nr.FunctionTemplates.Index = @enumFromInt(i);
                _ = try ti.instantiateNonGenericFn(ft_idx, &ft_item);
            }
        }
    }

    // -----------------------------------------------------------------------
    // Function instantiation
    // -----------------------------------------------------------------------

    fn instantiateNonGenericFn(ti: *TypeInferer, ft_idx: nr.FunctionTemplates.Index, fn_template: *const nr.FunctionTemplate) !DkType {
        assert(fn_template.typevar_count == 0);

        const params = fn_template.params(&ti.di.params_or_members);
        var scratch: [MAX_NUM_FUNCTION_PARAMS]DkType = undefined;
        for (params, 0..) |param, i| {
            switch (param.type_) {
                .CONCRETE => |decl_idx| scratch[i] = dkTypeFromBuiltinDecl(decl_idx) orelse DkType.UNKNOWN,
                .TYPEVAR => unreachable,
                .UNRESOLVED => scratch[i] = DkType.UNKNOWN,
            }
        }
        const param_types = scratch[0..fn_template.param_count];
        return try ti.actuallyInstantiateFunction(ft_idx, fn_template, param_types, DkType.UNKNOWN);
    }

    fn instantiateFunction(ti: *TypeInferer, fn_call_ast_idx: AstNodeIndex, ft_idx: nr.FunctionTemplates.Index, argument_types: []const DkType) !DkType {
        const fn_template = &ti.di.fn_templates.items[@intFromEnum(ft_idx)];

        if (fn_template.param_count != argument_types.len) {
            try ti.addError(.{ .wrong_num_fun_args = .{ .ast_node = fn_call_ast_idx, .actual = @intCast(argument_types.len), .expected = fn_template.param_count } });
            return error.WrongNumberOfFunctionArguments;
        }

        // Check for existing instance
        if (ti.fn_instances.getPtr(.{ .template_idx = ft_idx, .param_types = argument_types })) |existing| {
            if (existing.return_type == DkType.UNKNOWN) {
                try ti.addError(.{ .return_type_missing_for_recursive_fn = fn_template.ast_idx });
                return DkType.ERROR;
            }
            return existing.return_type;
        }

        // Typecheck params against template
        const params = fn_template.params(&ti.di.params_or_members);
        for (params, argument_types, 0..) |param, arg_type, i| {
            const fn_call_node = ti.ast.get(fn_call_ast_idx);
            var arg_iter = fn_call_node.children(ti.ast);
            var fn_argument_node_idx: AstNodeIndex = fn_call_ast_idx;
            for (0..i + 1) |_|
                fn_argument_node_idx = arg_iter.nextIdx() orelse fn_call_ast_idx;

            switch (param.type_) {
                .TYPEVAR => {}, // generic — will be resolved by arg_type
                .CONCRETE => |decl_idx| {
                    const expected_type = dkTypeFromBuiltinDecl(decl_idx) orelse DkType.UNKNOWN;
                    if (expected_type != arg_type and arg_type != DkType.ERROR) {
                        try ti.addError(.{ .wrong_type = .{ .ast_node = fn_argument_node_idx, .expected = expected_type.langName(), .actual = arg_type } });
                        return DkType.ERROR;
                    }
                },
                .UNRESOLVED => {},
            }
        }

        try ti.fn_instantiation_ast_idx_stack.append(ti.gpa, fn_call_ast_idx);
        defer {
            _ = ti.fn_instantiation_ast_idx_stack.pop();
        }

        return ti.actuallyInstantiateFunction(ft_idx, fn_template, argument_types, DkType.UNKNOWN);
    }

    fn actuallyInstantiateFunction(ti: *TypeInferer, ft_idx: nr.FunctionTemplates.Index, fn_template: *const nr.FunctionTemplate, param_types: []const DkType, return_type: DkType) !DkType {
        assert(param_types.len == fn_template.param_count);

        const params = fn_template.params(&ti.di.params_or_members);

        // Save and restore per-instantiation state
        const saved_decl_map = ti.decl_map;
        const saved_scope = ti.current_scope;
        ti.decl_map = .empty;
        defer {
            ti.decl_map.deinit(ti.gpa);
            ti.decl_map = saved_decl_map;
            ti.current_scope = saved_scope;
        }

        // Arena-allocate function name
        const fn_ast_node = ti.ast.get(fn_template.ast_idx);
        const fn_name = try ti.ft.arena.allocator().dupe(u8, ti.tokens[fn_ast_node.token_index].str(ti.source));

        // Create body scope
        const scope_idx: ScopeIndex = @intCast(ti.ft.scopes.items.len);
        try ti.ft.scopes.append(ti.gpa, .{ .kind = .FN, .parent_scope = 0 });
        ti.current_scope = scope_idx;

        // Create result VarDecl
        const result_decl_idx: VarDeclIndex = @intCast(ti.ft.var_decls.items.len);
        try ti.ft.var_decls.append(ti.gpa, .{
            .name = try ti.ft.arena.allocator().dupe(u8, "result"),
            .type_ = return_type,
            .kind = .RESULT,
            .parent_scope = scope_idx,
        });
        ti.ft.scopes.items[scope_idx].first_decl = result_decl_idx;

        // Find the result DeclIndex from name resolution (it's a RESULT_VARIABLE in the fn's scope)
        // The result decl was registered with ast_node_idx = fn node_idx
        const result_decl_search = blk: {
            for (ti.di.declarations.items[1..], 1..) |decl, idx| {
                if (decl.kind == .RESULT_VARIABLE and decl.ast_node_idx == fn_template.ast_idx)
                    break :blk @as(DeclIndex, @enumFromInt(idx));
            }
            unreachable;
        };
        try ti.decl_map.put(ti.gpa, result_decl_search, result_decl_idx);

        // Create param VarDecls
        const params_start: VarDeclIndex = @intCast(ti.ft.var_decls.items.len);
        for (params, param_types) |param, ptype| {
            const param_name = try ti.ft.arena.allocator().dupe(u8, ti.tokens[param.name_token_idx].str(ti.source));
            const var_decl_idx: VarDeclIndex = @intCast(ti.ft.var_decls.items.len);
            try ti.ft.var_decls.append(ti.gpa, .{
                .name = param_name,
                .type_ = ptype,
                .kind = .FN_PARAM,
                .parent_scope = scope_idx,
            });

            // Find param's DeclIndex
            const param_decl_idx = blk: {
                for (ti.di.declarations.items[1..], 1..) |decl, idx| {
                    if (decl.kind == .FN_PARAM and decl.name_token_idx == param.name_token_idx)
                        break :blk @as(DeclIndex, @enumFromInt(idx));
                }
                unreachable;
            };
            try ti.decl_map.put(ti.gpa, param_decl_idx, var_decl_idx);
        }
        const params_end: VarDeclIndex = @intCast(ti.ft.var_decls.items.len);

        // Store param types in the shared types array
        const first_param_type_idx = ti.fn_instances.param_types.items.len;
        for (param_types) |pt|
            _ = try ti.fn_instances.param_types.append(pt);
        const stored_param_types = Types.IndexRange.create(first_param_type_idx, fn_template.param_count);

        // Create FnDecl
        const fn_decl_idx: FnDeclIndex = @intCast(ti.ft.fn_decls.items.len);
        try ti.ft.fn_decls.append(ti.gpa, .{
            .name = fn_name,
            .return_type = return_type,
            .params = .{ .start = params_start, .end = params_end },
            .body_scope = scope_idx,
        });

        // Register instance (incomplete — return_type may be UNKNOWN)
        const res = try ti.fn_instances.put(
            .{ .template_idx = ft_idx, .param_types = stored_param_types },
            .{
                .fn_template_idx = ft_idx,
                .param_count = fn_template.param_count,
                .first_param_type_idx = @enumFromInt(first_param_type_idx),
                .return_type = return_type,
                .fn_decl_idx = fn_decl_idx,
            },
        );
        const inst = res.value_ptr;
        try ti.fn_instantiation_stack.append(ti.gpa, res.index);
        defer {
            _ = ti.fn_instantiation_stack.pop();
        }

        // Infer body
        ti.inferBlockInto(fn_template.body_ast_idx, scope_idx);

        // Resolve return type from result variable
        const result_var = ti.ft.var_decls.items[result_decl_idx];
        const actual_return_type = result_var.type_;
        if (inst.return_type == DkType.UNKNOWN) {
            inst.return_type = if (actual_return_type == DkType.UNKNOWN) DkType.VOID else actual_return_type;
        }
        ti.ft.fn_decls.items[fn_decl_idx].return_type = inst.return_type;

        return inst.return_type;
    }

    // -----------------------------------------------------------------------
    // Struct instantiation
    // -----------------------------------------------------------------------

    fn instantiateStruct(ti: *TypeInferer, st_idx: nr.StructTemplates.Index, member_types: []const DkType) !DkType {
        const template = &ti.di.struct_templates.items[@intFromEnum(st_idx)];

        // Check for existing instance with same types
        for (ti.struct_instances.items, 0..) |si, i| {
            if (si.template_idx != st_idx) continue;
            if (si.member_count != member_types.len) continue;
            const existing_types = ti.types.slice(si.first_member_type_idx, si.member_count);
            if (std.mem.eql(DkType, existing_types, member_types))
                return DkType.fromStructInstance(@intCast(i));
        }

        // Create new instance
        const first_member_type_idx = ti.types.items.len;
        for (member_types) |mt|
            _ = try ti.types.append(mt);

        const idx = ti.struct_instances.items.len;
        try ti.struct_instances.append(ti.gpa, .{
            .template_idx = st_idx,
            .member_count = template.member_count,
            .first_member_type_idx = @enumFromInt(first_member_type_idx),
        });

        // Create StructDecl in FtAst
        const arena = ti.ft.arena.allocator();
        const members = template.members(&ti.di.params_or_members);

        const struct_name = try arena.dupe(u8, ti.tokens[ti.di.declarations.items[@intFromEnum(template.decl_idx)].name_token_idx].str(ti.source));

        // Create member VarDecls
        const members_start: VarDeclIndex = @intCast(ti.ft.var_decls.items.len);
        for (members, member_types[0..template.member_count]) |m, mt| {
            const member_name = try arena.dupe(u8, ti.tokens[m.name_token_idx].str(ti.source));
            try ti.ft.var_decls.append(ti.gpa, .{
                .name = member_name,
                .type_ = mt,
                .kind = .VAR,
            });
        }
        const members_end: VarDeclIndex = @intCast(ti.ft.var_decls.items.len);

        try ti.ft.struct_decls.append(ti.gpa, .{
            .name = struct_name,
            .members = .{ .start = members_start, .end = members_end },
            .generic = template.typevar_count > 0,
        });

        return DkType.fromStructInstance(@intCast(idx));
    }

    // -----------------------------------------------------------------------
    // Block inference + lowering
    // -----------------------------------------------------------------------

    fn inferBlockInto(ti: *TypeInferer, block_ast_idx: AstNodeIndex, scope_idx: ScopeIndex) void {
        const block_node = ti.ast.get(block_ast_idx);
        assert(block_node.tag == .BLOCK);

        var child_iter = block_node.children(ti.ast);
        var prev_stmt_idx: StamentIndex = 0;

        while (child_iter.nextIdx()) |child_ast_idx| {
            const stmt_idx = ti.inferStatement(child_ast_idx, scope_idx) catch continue;

            if (prev_stmt_idx == 0)
                ti.ft.scopes.items[scope_idx].first_statement = stmt_idx
            else
                ti.ft.statements.items[prev_stmt_idx].next_sibling = stmt_idx;

            prev_stmt_idx = stmt_idx;
        }
    }

    fn inferBlock(ti: *TypeInferer, block_ast_idx: AstNodeIndex, kind: @FieldType(ft_ast.Scope, "kind"), parent_scope: ScopeIndex) !ScopeIndex {
        const scope_idx: ScopeIndex = @intCast(ti.ft.scopes.items.len);
        try ti.ft.scopes.append(ti.gpa, .{ .kind = kind, .parent_scope = parent_scope });
        ti.inferBlockInto(block_ast_idx, scope_idx);
        return scope_idx;
    }

    // -----------------------------------------------------------------------
    // Statement inference + lowering
    // -----------------------------------------------------------------------

    fn inferStatement(ti: *TypeInferer, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) TypeInfererException!StamentIndex {
        const node = ti.ast.get(ast_idx);
        return switch (node.tag) {
            .DECLARATION => ti.inferDecl(ast_idx, parent_scope),
            .ASSIGNMENT => ti.inferAssignment(ast_idx),
            .CALL_OR_INST => ti.inferCallOrInstStmt(ast_idx),
            .IF => ti.inferIf(ast_idx, parent_scope),
            .WHILE => ti.inferWhile(ast_idx, parent_scope),
            .BLOCK => {
                const sub_scope = try ti.inferBlock(ast_idx, .IF, parent_scope); // reuse IF kind for nested blocks
                _ = sub_scope;
                // No statement emitted for bare blocks — they just create a scope
                return error.OutOfMemory; // FIXME: this is wrong, but bare blocks in statement position are unusual
            },
            else => unreachable,
        };
    }

    fn checkStructAnnotation(ti: *TypeInferer, ast_idx: AstNodeIndex, annotation_decl_idx: DeclIndex, rhs_type: DkType) !void {
        if (rhs_type == DkType.ERROR) return;
        if (!rhs_type.isStruct()) {
            try ti.addError(.{ .member_access_on_non_struct = .{ .ast_node = ast_idx, .actual = rhs_type } });
            return;
        }
        const rhs_template = ti.struct_instances.items[rhs_type.structInstanceIdx()].template_idx;
        for (ti.di.struct_templates.items[1..], 1..) |template, i| {
            if (@as(nr.StructTemplates.Index, @enumFromInt(i)) == rhs_template) {
                if (template.decl_idx != annotation_decl_idx)
                    try ti.addError(.{ .type_mismatch = .{ .ast_node = ast_idx, .lhs = DkType.UNKNOWN, .rhs = rhs_type } });
                return;
            }
        }
    }

    // -----------------------------------------------------------------------
    // Array inference helpers
    // -----------------------------------------------------------------------

    const ArrayAnnotation = struct {
        ndim: u8,
        dims: [ft_ast.MAX_ARRAY_NDIM]?u16, // null = INFER_DIM
        elem_type: DkType,
    };

    fn resolveArrayAnnotation(ti: *TypeInferer, type_node_idx: AstNodeIndex) ?ArrayAnnotation {
        const type_node = ti.ast.get(type_node_idx);
        assert(type_node.tag == .TYPE);

        const elem_decl_idx = ti.di.name_resolution[type_node_idx];
        const elem_type = dkTypeFromBuiltinDecl(elem_decl_idx) orelse return null;

        const shape_node_idx = type_node.first_child;
        const shape_node = ti.ast.get(shape_node_idx);
        assert(shape_node.tag == .ARRAY_SHAPE);

        var annotation = ArrayAnnotation{
            .ndim = 0,
            .dims = .{null} ** ft_ast.MAX_ARRAY_NDIM,
            .elem_type = elem_type,
        };
        var child_iter = shape_node.children(ti.ast);
        while (child_iter.nextIdx()) |dim_idx| {
            const dim_node = ti.ast.get(dim_idx);
            if (dim_node.tag == .INFER_DIM) {
                annotation.dims[annotation.ndim] = null;
            } else {
                const size = std.fmt.parseInt(u16, ti.tokens[dim_node.token_index].str(ti.source), 10) catch unreachable;
                annotation.dims[annotation.ndim] = size;
            }
            annotation.ndim += 1;
        }
        return annotation;
    }

    fn getOrCreateArrayInstance(ti: *TypeInferer, shape: []const u16, elem_type: DkType) !DkType {
        // TODO: replace linear scan with hash map for large programs
        const ndim: u8 = @intCast(shape.len);
        for (ti.ft.array_instances.items, 0..) |ai, i| {
            if (ai.ndim != ndim) continue;
            if (ai.elem_type != elem_type) continue;
            if (!std.mem.eql(u16, ai.shape[0..ndim], shape)) continue;
            return DkType.fromArrayInstance(@intCast(i));
        }
        const idx = ti.ft.array_instances.items.len;
        var new_instance = ft_ast.ArrayInstance{
            .elem_type = elem_type,
            .ndim = ndim,
            .shape = .{0} ** ft_ast.MAX_ARRAY_NDIM,
        };
        @memcpy(new_instance.shape[0..ndim], shape);
        try ti.ft.array_instances.append(ti.gpa, new_instance);
        return DkType.fromArrayInstance(@intCast(idx));
    }

    fn setArrayExprType(ti: *TypeInferer, expr_idx: ExpressionIndex, array_type: DkType) !void {
        ti.ft.expressions.items[expr_idx].type_ = array_type;
        const ai = ti.ft.array_instances.items[array_type.arrayInstanceIdx()];
        if (ai.ndim == 1) {
            var child_expr = ti.ft.expressions.items[expr_idx].kind.ARRAY_LIT.elems_start;
            while (child_expr != 0) {
                const next = ti.ft.expressions.items[child_expr].next_sibling;
                if (ti.ft.expressions.items[child_expr].kind == .FILL)
                    ti.ft.expressions.items[child_expr].type_ = ai.elem_type;
                child_expr = next;
            }
        } else {
            const inner_type = try ti.getOrCreateArrayInstance(ai.shape[1..ai.ndim], ai.elem_type);
            var child_expr = ti.ft.expressions.items[expr_idx].kind.ARRAY_LIT.elems_start;
            while (child_expr != 0) {
                const next = ti.ft.expressions.items[child_expr].next_sibling;
                switch (ti.ft.expressions.items[child_expr].kind) {
                    .ARRAY_LIT => try ti.setArrayExprType(child_expr, inner_type),
                    .FILL => ti.ft.expressions.items[child_expr].type_ = inner_type,
                    else => {},
                }
                child_expr = next;
            }
        }
    }

    const ArrayLitResult = struct {
        expr_idx: ExpressionIndex,
        shape: ArrayShape,
        elem_type: DkType,
    };

    fn inferArrayLit(ti: *TypeInferer, ast_idx: AstNodeIndex) !ArrayLitResult {
        const shape = arrayShapeFromLit(ti.ast, ast_idx);
        if (shape.extra_fill_node != 0)
            try ti.addError(.{ .multiple_fills_in_array_literal = shape.extra_fill_node });

        const node = ti.ast.get(ast_idx);
        var elem_type: DkType = DkType.UNKNOWN;
        var first_inner_shape: ?ArrayShape = null;
        var first_elem_expr: ExpressionIndex = 0;
        var prev_elem_expr: ExpressionIndex = 0;

        var child_iter = node.children(ti.ast);
        while (child_iter.nextIdx()) |child_ast_idx| {
            const child_node = ti.ast.get(child_ast_idx);
            if (child_node.tag == .FILL) {
                const fill_val_ast_idx = child_node.first_child;
                const fill_val_node = ti.ast.get(fill_val_ast_idx);
                var fill_val_expr: ExpressionIndex = 0;
                var fill_elem_type: DkType = DkType.UNKNOWN;
                if (fill_val_node.tag == .ARRAY_LIT) {
                    const inner = try ti.inferArrayLit(fill_val_ast_idx);
                    fill_val_expr = inner.expr_idx;
                    fill_elem_type = inner.elem_type;
                } else {
                    fill_val_expr = try ti.inferExpr(fill_val_ast_idx);
                    fill_elem_type = ti.ft.expressions.items[fill_val_expr].type_;
                }
                if (elem_type == DkType.UNKNOWN) {
                    elem_type = fill_elem_type;
                } else if (fill_elem_type != DkType.UNKNOWN and fill_elem_type != DkType.ERROR and elem_type != fill_elem_type) {
                    try ti.addError(.{ .inconsistent_elem_types_in_array_lit = .{ .ast_node = child_ast_idx, .expected = elem_type, .actual = fill_elem_type } });
                }
                const fill_expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
                try ti.ft.expressions.append(ti.gpa, .{
                    .type_ = DkType.UNKNOWN,
                    .kind = .{ .FILL = .{ .value = fill_val_expr } },
                });
                if (first_elem_expr == 0)
                    first_elem_expr = fill_expr_idx
                else
                    ti.ft.expressions.items[prev_elem_expr].next_sibling = fill_expr_idx;
                prev_elem_expr = fill_expr_idx;
            } else if (child_node.tag == .ARRAY_LIT) {
                const inner = try ti.inferArrayLit(child_ast_idx);
                if (first_inner_shape) |ref_shape| {
                    for (0..@min(ref_shape.ndim, inner.shape.ndim)) |d| {
                        if (!ref_shape.dims[d].has_fill and !inner.shape.dims[d].has_fill and
                            ref_shape.dims[d].count != inner.shape.dims[d].count)
                        {
                            try ti.addError(.{ .inconsistent_inner_dim = .{ .ast_node = child_ast_idx, .expected = ref_shape.dims[d].count, .actual = inner.shape.dims[d].count } });
                            break;
                        }
                    }
                } else {
                    first_inner_shape = inner.shape;
                }
                if (elem_type == DkType.UNKNOWN) {
                    elem_type = inner.elem_type;
                } else if (inner.elem_type != DkType.UNKNOWN and inner.elem_type != DkType.ERROR and elem_type != inner.elem_type) {
                    try ti.addError(.{ .inconsistent_elem_types_in_array_lit = .{ .ast_node = child_ast_idx, .expected = elem_type, .actual = inner.elem_type } });
                }
                if (first_elem_expr == 0)
                    first_elem_expr = inner.expr_idx
                else
                    ti.ft.expressions.items[prev_elem_expr].next_sibling = inner.expr_idx;
                prev_elem_expr = inner.expr_idx;
            } else {
                const child_expr = try ti.inferExpr(child_ast_idx);
                const child_type = ti.ft.expressions.items[child_expr].type_;
                if (elem_type == DkType.UNKNOWN) {
                    elem_type = child_type;
                } else if (child_type != DkType.UNKNOWN and child_type != DkType.ERROR and elem_type != child_type) {
                    try ti.addError(.{ .inconsistent_elem_types_in_array_lit = .{ .ast_node = child_ast_idx, .expected = elem_type, .actual = child_type } });
                }
                if (first_elem_expr == 0)
                    first_elem_expr = child_expr
                else
                    ti.ft.expressions.items[prev_elem_expr].next_sibling = child_expr;
                prev_elem_expr = child_expr;
            }
        }

        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
        try ti.ft.expressions.append(ti.gpa, .{
            .type_ = DkType.UNKNOWN,
            .kind = .{ .ARRAY_LIT = .{ .elems_start = first_elem_expr } },
        });
        return .{ .expr_idx = expr_idx, .shape = shape, .elem_type = elem_type };
    }

    fn inferArrayAccess(ti: *TypeInferer, ast_idx: AstNodeIndex) !ExpressionIndex {
        const node = ti.ast.get(ast_idx);
        assert(node.tag == .ARRAY_ACCESS);

        const base_ast_idx = node.first_child;
        assert(base_ast_idx != 0);
        const index_args_ast_idx = ti.ast.get(base_ast_idx).next_sibling;
        assert(index_args_ast_idx != 0);

        const base_expr = try ti.inferExpr(base_ast_idx);
        const base_type = ti.ft.expressions.items[base_expr].type_;

        if (base_type == DkType.ERROR) {
            const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
            try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
            return expr_idx;
        }
        if (!base_type.isArray()) {
            try ti.addError(.{ .symbol_is_not_array = .{ .ast_node = ast_idx, .actual = base_type } });
            const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
            try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
            return expr_idx;
        }

        const ai = ti.ft.array_instances.items[base_type.arrayInstanceIdx()];
        var first_idx_expr: ExpressionIndex = 0;
        var prev_idx_expr: ExpressionIndex = 0;
        var idx_count: u8 = 0;

        const index_args_node = ti.ast.get(index_args_ast_idx);
        var idx_iter = index_args_node.children(ti.ast);
        while (idx_iter.nextIdx()) |idx_ast_idx| {
            const idx_expr = try ti.inferExpr(idx_ast_idx);
            const idx_type = ti.ft.expressions.items[idx_expr].type_;
            try ti.expectType(idx_ast_idx, idx_type, DkType.INT);
            if (first_idx_expr == 0)
                first_idx_expr = idx_expr
            else
                ti.ft.expressions.items[prev_idx_expr].next_sibling = idx_expr;
            prev_idx_expr = idx_expr;
            idx_count += 1;
        }

        if (idx_count != ai.ndim)
            try ti.addError(.{ .wrong_num_fun_args = .{ .ast_node = ast_idx, .expected = ai.ndim, .actual = idx_count } });

        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
        try ti.ft.expressions.append(ti.gpa, .{
            .type_ = ai.elem_type,
            .kind = .{ .ARRAY_ACCESS = .{ .base = base_expr, .indices_start = first_idx_expr } },
        });
        return expr_idx;
    }

    fn inferDecl(ti: *TypeInferer, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) !StamentIndex {
        const node = ti.ast.get(ast_idx);
        assert(node.tag == .DECLARATION);

        const first_child_idx = node.first_child;
        assert(first_child_idx != 0);
        const first_child = ti.ast.get(first_child_idx);

        var rhs_ast_idx: AstNodeIndex = undefined;
        var type_decl_idx: DeclIndex = .NONE;
        var has_array_annotation = false;
        if (first_child.tag == .TYPE) {
            rhs_ast_idx = first_child.next_sibling;
            type_decl_idx = ti.di.name_resolution[first_child_idx];
            if (first_child.first_child != 0 and ti.ast.get(first_child.first_child).tag == .ARRAY_SHAPE)
                has_array_annotation = true;
        } else {
            rhs_ast_idx = first_child_idx;
        }

        const is_array_lit_rhs = ti.ast.get(rhs_ast_idx).tag == .ARRAY_LIT;

        var var_type: DkType = DkType.UNKNOWN;
        var rhs_expr: ExpressionIndex = 0;

        if (has_array_annotation and is_array_lit_rhs) {
            if (ti.resolveArrayAnnotation(first_child_idx)) |ann| {
                const lit_result = try ti.inferArrayLit(rhs_ast_idx);
                rhs_expr = lit_result.expr_idx;
                const shape = lit_result.shape;
                if (shape.ndim != ann.ndim) {
                    try ti.addError(.{ .array_shape_mismatch = .{ .ast_node = rhs_ast_idx, .expected = ann.ndim, .actual = shape.ndim } });
                    var_type = DkType.ERROR;
                } else {
                    var final_shape: [ft_ast.MAX_ARRAY_NDIM]u16 = .{0} ** ft_ast.MAX_ARRAY_NDIM;
                    var shape_ok = true;
                    for (0..ann.ndim) |i| {
                        if (ann.dims[i]) |concrete| {
                            if (shape.dims[i].has_fill) {
                                if (shape.dims[i].count > concrete) {
                                    try ti.addError(.{ .array_shape_mismatch = .{ .ast_node = rhs_ast_idx, .expected = concrete, .actual = shape.dims[i].count } });
                                    shape_ok = false;
                                }
                            } else if (shape.dims[i].count != concrete) {
                                try ti.addError(.{ .array_shape_mismatch = .{ .ast_node = rhs_ast_idx, .expected = concrete, .actual = shape.dims[i].count } });
                                shape_ok = false;
                            }
                            final_shape[i] = concrete;
                        } else {
                            if (shape.dims[i].has_fill) {
                                try ti.addError(.{ .cannot_infer_array_size = rhs_ast_idx });
                                shape_ok = false;
                            }
                            final_shape[i] = shape.dims[i].count;
                        }
                    }
                    if (shape_ok) {
                        const array_type = try ti.getOrCreateArrayInstance(final_shape[0..ann.ndim], ann.elem_type);
                        try ti.setArrayExprType(rhs_expr, array_type);
                        var_type = array_type;
                    } else {
                        var_type = DkType.ERROR;
                    }
                }
            } else {
                // struct array annotation or unresolvable — fall through to plain inferExpr
                rhs_expr = try ti.inferExpr(rhs_ast_idx);
                var_type = ti.ft.expressions.items[rhs_expr].type_;
            }
        } else if (!has_array_annotation and is_array_lit_rhs) {
            const lit_result = try ti.inferArrayLit(rhs_ast_idx);
            rhs_expr = lit_result.expr_idx;
            const shape = lit_result.shape;
            const elem_type = lit_result.elem_type;
            var shape_ok = true;
            for (0..shape.ndim) |i| {
                if (shape.dims[i].has_fill) {
                    try ti.addError(.{ .cannot_infer_array_size = rhs_ast_idx });
                    shape_ok = false;
                    break;
                }
            }
            if (shape_ok) {
                var shape_counts: [ft_ast.MAX_ARRAY_NDIM]u16 = .{0} ** ft_ast.MAX_ARRAY_NDIM;
                for (0..shape.ndim) |i|
                    shape_counts[i] = shape.dims[i].count;
                const array_type = try ti.getOrCreateArrayInstance(shape_counts[0..shape.ndim], elem_type);
                try ti.setArrayExprType(rhs_expr, array_type);
                var_type = array_type;
            } else {
                var_type = DkType.ERROR;
            }
        } else {
            rhs_expr = try ti.inferExpr(rhs_ast_idx);
            const rhs_type = ti.ft.expressions.items[rhs_expr].type_;
            var_type = rhs_type;
            if (type_decl_idx != .NONE) {
                if (dkTypeFromBuiltinDecl(type_decl_idx)) |annotation_type| {
                    var_type = annotation_type;
                    if (rhs_type != annotation_type and rhs_type != DkType.ERROR)
                        try ti.addError(.{ .type_mismatch = .{ .ast_node = rhs_ast_idx, .lhs = annotation_type, .rhs = rhs_type } });
                } else if (!has_array_annotation) {
                    const decl = ti.di.declarations.items[@intFromEnum(type_decl_idx)];
                    if (decl.kind == .STRUCT)
                        try ti.checkStructAnnotation(rhs_ast_idx, type_decl_idx, rhs_type);
                }
            }
        }

        const var_name = try ti.ft.arena.allocator().dupe(u8, ti.tokens[node.token_index].str(ti.source));

        const var_decl_idx: VarDeclIndex = @intCast(ti.ft.var_decls.items.len);
        try ti.ft.var_decls.append(ti.gpa, .{
            .name = var_name,
            .type_ = var_type,
            .kind = .VAR,
            .parent_scope = parent_scope,
        });

        ti.ft.var_decls.items[var_decl_idx].next_sibling = ti.ft.scopes.items[parent_scope].first_decl;
        ti.ft.scopes.items[parent_scope].first_decl = var_decl_idx;

        const decl_idx = ti.di.name_resolution[ast_idx];
        if (decl_idx != .NONE)
            try ti.decl_map.put(ti.gpa, decl_idx, var_decl_idx);

        const stmt_idx: StamentIndex = @intCast(ti.ft.statements.items.len);
        try ti.ft.statements.append(ti.gpa, .{
            .kind = .{ .VAR_DECL = .{ .var_decl_idx = var_decl_idx, .rhs = rhs_expr } },
        });
        return stmt_idx;
    }

    fn inferAssignment(ti: *TypeInferer, ast_idx: AstNodeIndex) !StamentIndex {
        const node = ti.ast.get(ast_idx);
        const op_token = ti.tokens[node.token_index];

        const lhs_ast_idx = node.first_child;
        assert(lhs_ast_idx != 0);
        const lhs_ast = ti.ast.get(lhs_ast_idx);
        const rhs_ast_idx = lhs_ast.next_sibling;
        assert(rhs_ast_idx != 0);

        // Handle member access on LHS
        if (lhs_ast.tag == .MEMBER_ACCESS)
            return ti.inferMemberAssignment(ast_idx, lhs_ast_idx, rhs_ast_idx, op_token.tag);

        // Regular variable assignment
        const decl_idx = ti.di.name_resolution[lhs_ast_idx];
        assert(decl_idx != .NONE);
        const var_decl_idx = ti.decl_map.get(decl_idx).?;
        const var_decl = ti.ft.var_decls.items[var_decl_idx];

        // Check immutability of fn params
        if (var_decl.kind == .FN_PARAM) {
            try ti.addError(.{ .fn_params_are_immutable = .{ .declaration = ti.di.declarations.items[@intFromEnum(decl_idx)].name_token_idx, .usage = lhs_ast.token_index } });
            return error.OutOfMemory; // abort this statement
        }

        if (var_decl.kind == .RESULT) {
            const rhs_expr = try ti.inferExpr(rhs_ast_idx);
            const rhs_type = ti.ft.expressions.items[rhs_expr].type_;

            // Update result type
            if (ti.ft.var_decls.items[var_decl_idx].type_ == DkType.UNKNOWN) {
                ti.ft.var_decls.items[var_decl_idx].type_ = rhs_type;
            } else if (ti.ft.var_decls.items[var_decl_idx].type_ != rhs_type and rhs_type != DkType.ERROR) {
                try ti.addError(.{ .result_type_mismatch = .{ .ast_node = ast_idx, .lhs = ti.ft.var_decls.items[var_decl_idx].type_, .rhs = rhs_type } });
            }

            const stmt_idx: StamentIndex = @intCast(ti.ft.statements.items.len);
            try ti.ft.statements.append(ti.gpa, .{
                .kind = .{ .RESULT_ASSIGN = .{ .type_ = rhs_type, .rhs = rhs_expr } },
            });
            return stmt_idx;
        }

        // Normal variable assignment
        const assign_kind: AssignmentKind = tokenToAssignmentKind(op_token.tag);

        // Type check
        const lhs_type = var_decl.type_;
        const rhs_type = blk: {
            const rhs_expr_idx = try ti.inferExpr(rhs_ast_idx);
            break :blk .{ rhs_expr_idx, ti.ft.expressions.items[rhs_expr_idx].type_ };
        };
        const rhs_expr = rhs_type[0];
        const rhs_t = rhs_type[1];

        if (rhs_t != DkType.ERROR and lhs_type != DkType.ERROR) {
            switch (assign_kind) {
                .ASSIGN => {
                    if (lhs_type != rhs_t)
                        try ti.addError(.{ .type_mismatch = .{ .ast_node = ast_idx, .lhs = lhs_type, .rhs = rhs_t } });
                },
                .PLUS, .MINUS, .MULT, .DIV => {
                    if (lhs_type != DkType.INT and lhs_type != DkType.FLOAT)
                        try ti.addError(.{ .wrong_type = .{ .ast_node = lhs_ast_idx, .actual = lhs_type, .expected = "LHS must be of number type" } });
                    if (rhs_t != DkType.INT and rhs_t != DkType.FLOAT)
                        try ti.addError(.{ .wrong_type = .{ .ast_node = rhs_ast_idx, .actual = rhs_t, .expected = "RHS must be of number type" } });
                    if (lhs_type != rhs_t)
                        try ti.addError(.{ .type_mismatch = .{ .ast_node = ast_idx, .lhs = lhs_type, .rhs = rhs_t } });
                },
            }
        }

        const stmt_idx: StamentIndex = @intCast(ti.ft.statements.items.len);
        try ti.ft.statements.append(ti.gpa, .{
            .kind = .{ .ASSIGNMENT = .{
                .var_decl_idx = var_decl_idx,
                .type_ = rhs_t,
                .kind = assign_kind,
                .rhs = rhs_expr,
            } },
        });
        return stmt_idx;
    }

    fn inferMemberAssignment(ti: *TypeInferer, _: AstNodeIndex, lhs_ast_idx: AstNodeIndex, rhs_ast_idx: AstNodeIndex, op_tag: tok.Token.Tag) !StamentIndex {
        const lhs_node = ti.ast.get(lhs_ast_idx);
        assert(lhs_node.tag == .MEMBER_ACCESS);

        // Infer base expression
        const base_expr = try ti.inferExpr(lhs_node.first_child);
        const base_type = ti.ft.expressions.items[base_expr].type_;

        if (!base_type.isStruct()) {
            try ti.addError(.{ .member_access_on_non_struct = .{ .ast_node = lhs_ast_idx, .actual = base_type } });
            return error.OutOfMemory;
        }

        const si = ti.struct_instances.items[base_type.structInstanceIdx()];
        const template = &ti.di.struct_templates.items[@intFromEnum(si.template_idx)];
        const members = template.members(&ti.di.params_or_members);
        const field_name = ti.tokens[lhs_node.token_index].str(ti.source);

        // Find member
        var member_idx: ?u8 = null;
        for (members, 0..) |m, i| {
            if (std.mem.eql(u8, ti.tokens[m.name_token_idx].str(ti.source), field_name)) {
                member_idx = @intCast(i);
                break;
            }
        }

        if (member_idx == null) {
            try ti.addError(.{ .unknown_field = .{ .ast_node = lhs_ast_idx, .field_name = lhs_node.token_index } });
            return error.OutOfMemory;
        }

        const rhs_expr = try ti.inferExpr(rhs_ast_idx);
        const assign_kind = tokenToAssignmentKind(op_tag);

        const stmt_idx: StamentIndex = @intCast(ti.ft.statements.items.len);
        try ti.ft.statements.append(ti.gpa, .{
            .kind = .{ .MEMBER_ASSIGN = .{
                .base = base_expr,
                .member_idx = member_idx.?,
                .kind = assign_kind,
                .rhs = rhs_expr,
            } },
        });
        return stmt_idx;
    }

    fn inferIf(ti: *TypeInferer, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) !StamentIndex {
        const node = ti.ast.get(ast_idx);
        const cte = node.conditionThenElse(ti.ast);

        const cond_expr = try ti.inferExpr(cte.cond_idx);
        const cond_type = ti.ft.expressions.items[cond_expr].type_;
        try ti.expectType(cte.cond_idx, cond_type, DkType.BOOL);

        const then_scope = try ti.inferBlock(cte.then_idx, .IF, parent_scope);
        const else_scope: ScopeIndex = if (cte.else_idx != 0)
            try ti.inferBlock(cte.else_idx, .ELSE, parent_scope)
        else
            0;

        const stmt_idx: StamentIndex = @intCast(ti.ft.statements.items.len);
        try ti.ft.statements.append(ti.gpa, .{
            .kind = .{ .IF_STMT = .{
                .condition = cond_expr,
                .then_scope = then_scope,
                .else_scope = else_scope,
            } },
        });
        return stmt_idx;
    }

    fn inferWhile(ti: *TypeInferer, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) !StamentIndex {
        const node = ti.ast.get(ast_idx);
        const cond_idx = node.first_child;
        const body_ast_idx = ti.ast.get(cond_idx).next_sibling;

        const cond_expr = try ti.inferExpr(cond_idx);
        const cond_type = ti.ft.expressions.items[cond_expr].type_;
        try ti.expectType(cond_idx, cond_type, DkType.BOOL);

        const body_scope = try ti.inferBlock(body_ast_idx, .WHILE, parent_scope);

        const stmt_idx: StamentIndex = @intCast(ti.ft.statements.items.len);
        try ti.ft.statements.append(ti.gpa, .{
            .kind = .{ .WHILE_LOOP = .{
                .condition = cond_expr,
                .body_scope = body_scope,
            } },
        });
        return stmt_idx;
    }

    fn inferCallOrInstStmt(ti: *TypeInferer, ast_idx: AstNodeIndex) !StamentIndex {
        const expr_idx = try ti.inferExpr(ast_idx);

        const stmt_idx: StamentIndex = @intCast(ti.ft.statements.items.len);
        try ti.ft.statements.append(ti.gpa, .{
            .kind = .{ .FN_CALL = expr_idx },
        });
        return stmt_idx;
    }

    // -----------------------------------------------------------------------
    // Expression inference + lowering
    // -----------------------------------------------------------------------

    fn inferExpr(ti: *TypeInferer, ast_idx: AstNodeIndex) TypeInfererException!ExpressionIndex {
        const node = ti.ast.get(ast_idx);
        return switch (node.tag) {
            .ATOM => ti.inferAtom(ast_idx),
            .BINARY_OP => ti.inferBinaryOp(ast_idx),
            .UNARY_OP => ti.inferUnaryOp(ast_idx),
            .CALL_OR_INST => ti.inferCallOrInst(ast_idx),
            .MEMBER_ACCESS => ti.inferMemberAccess(ast_idx),
            .ARRAY_ACCESS => ti.inferArrayAccess(ast_idx),
            else => unreachable,
        };
    }

    fn inferAtom(ti: *TypeInferer, ast_idx: AstNodeIndex) !ExpressionIndex {
        const node = ti.ast.get(ast_idx);
        const token = ti.tokens[node.token_index];
        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);

        switch (token.tag) {
            .INT_LIT => {
                const val = std.fmt.parseInt(i64, token.str(ti.source), 10) catch unreachable;
                try ti.ft.expressions.append(ti.gpa, .{
                    .type_ = DkType.INT,
                    .kind = .{ .LITERAL = .{ .int = val } },
                });
            },
            .FLOAT_LIT => {
                const val = std.fmt.parseFloat(f64, token.str(ti.source)) catch unreachable;
                try ti.ft.expressions.append(ti.gpa, .{
                    .type_ = DkType.FLOAT,
                    .kind = .{ .LITERAL = .{ .float = val } },
                });
            },
            .TRUE => {
                try ti.ft.expressions.append(ti.gpa, .{
                    .type_ = DkType.BOOL,
                    .kind = .{ .LITERAL = .{ .boolean = true } },
                });
            },
            .FALSE => {
                try ti.ft.expressions.append(ti.gpa, .{
                    .type_ = DkType.BOOL,
                    .kind = .{ .LITERAL = .{ .boolean = false } },
                });
            },
            .IDENTIFIER => {
                const decl_idx = ti.di.name_resolution[ast_idx];
                assert(decl_idx != .NONE);
                const var_decl_idx = ti.decl_map.get(decl_idx).?;
                const var_type = ti.ft.var_decls.items[var_decl_idx].type_;
                try ti.ft.expressions.append(ti.gpa, .{
                    .type_ = var_type,
                    .kind = .{ .VAR_REF = var_decl_idx },
                });
            },
            else => unreachable,
        }
        return expr_idx;
    }

    fn inferBinaryOp(ti: *TypeInferer, ast_idx: AstNodeIndex) !ExpressionIndex {
        const node = ti.ast.get(ast_idx);
        const token = ti.tokens[node.token_index];

        const lhs_ast_idx = node.first_child;
        const rhs_ast_idx = ti.ast.get(lhs_ast_idx).next_sibling;

        const lhs_expr = try ti.inferExpr(lhs_ast_idx);
        const rhs_expr = try ti.inferExpr(rhs_ast_idx);

        const lhs_type = ti.ft.expressions.items[lhs_expr].type_;
        const rhs_type = ti.ft.expressions.items[rhs_expr].type_;

        const op: BinaryOp = switch (token.tag) {
            // zig fmt: off
            .PLUS    => .ADD,
            .MINUS   => .SUB,
            .TIMES   => .MUL,
            .DIV     => .DIV,
            .POW     => .POW,
            .EQ      => .EQ,
            .NOT_EQ  => .NEQ,
            .LT      => .LT,
            .LE      => .LE,
            .GT      => .GT,
            .GE      => .GE,
            .AND     => .BOOL_AND,
            .OR      => .BOOL_OR,
            .XOR     => .BOOL_XOR,
            // zig fmt: on
            else => unreachable,
        };

        // Type checking
        var result_type: DkType = DkType.ERROR;
        if (lhs_type != DkType.ERROR and rhs_type != DkType.ERROR) {
            switch (token.tag) {
                .EQ, .NOT_EQ, .LT, .LE, .GT, .GE => {
                    if (lhs_type != rhs_type or lhs_type == DkType.UNKNOWN)
                        try ti.addError(.{ .type_mismatch = .{ .ast_node = ast_idx, .lhs = lhs_type, .rhs = rhs_type } })
                    else
                        result_type = DkType.BOOL;
                },
                .PLUS, .MINUS, .TIMES, .DIV, .POW => {
                    var err = false;
                    if (lhs_type != DkType.INT and lhs_type != DkType.FLOAT) {
                        try ti.addError(.{ .wrong_type = .{ .ast_node = lhs_ast_idx, .actual = lhs_type, .expected = "LHS must be of number type" } });
                        err = true;
                    }
                    if (rhs_type != DkType.INT and rhs_type != DkType.FLOAT) {
                        try ti.addError(.{ .wrong_type = .{ .ast_node = rhs_ast_idx, .actual = rhs_type, .expected = "RHS must be of number type" } });
                        err = true;
                    }
                    if (!err) {
                        if (lhs_type != rhs_type)
                            try ti.addError(.{ .type_mismatch = .{ .ast_node = ast_idx, .lhs = lhs_type, .rhs = rhs_type } })
                        else
                            result_type = lhs_type;
                    }
                },
                .AND, .OR, .XOR => {
                    var err = false;
                    if (lhs_type != DkType.BOOL) {
                        try ti.addError(.{ .wrong_type = .{ .ast_node = lhs_ast_idx, .actual = lhs_type, .expected = "LHS must be a boolean" } });
                        err = true;
                    }
                    if (rhs_type != DkType.BOOL) {
                        try ti.addError(.{ .wrong_type = .{ .ast_node = rhs_ast_idx, .actual = rhs_type, .expected = "RHS must be a boolean" } });
                        err = true;
                    }
                    if (!err)
                        result_type = DkType.BOOL;
                },
                else => unreachable,
            }
        }

        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
        try ti.ft.expressions.append(ti.gpa, .{
            .type_ = result_type,
            .kind = .{ .BINARY_OP = .{ .op = op, .lhs = lhs_expr, .rhs = rhs_expr } },
        });
        return expr_idx;
    }

    fn inferUnaryOp(ti: *TypeInferer, ast_idx: AstNodeIndex) !ExpressionIndex {
        const node = ti.ast.get(ast_idx);
        const token = ti.tokens[node.token_index];

        const operand_expr = try ti.inferExpr(node.first_child);
        const operand_type = ti.ft.expressions.items[operand_expr].type_;

        const op: UnaryOp = switch (token.tag) {
            .MINUS => .NEGATE,
            .NOT => .BOOL_NOT,
            else => unreachable,
        };

        var result_type: DkType = DkType.ERROR;
        if (operand_type != DkType.ERROR) {
            switch (token.tag) {
                .NOT => {
                    if (operand_type != DkType.BOOL)
                        try ti.addError(.{ .wrong_type = .{ .ast_node = node.first_child, .actual = operand_type, .expected = "Child expression must be a boolean" } })
                    else
                        result_type = DkType.BOOL;
                },
                .MINUS => {
                    if (operand_type != DkType.INT and operand_type != DkType.FLOAT)
                        try ti.addError(.{ .wrong_type = .{ .ast_node = node.first_child, .actual = operand_type, .expected = "Child expression must be a number" } })
                    else
                        result_type = operand_type;
                },
                else => unreachable,
            }
        }

        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
        try ti.ft.expressions.append(ti.gpa, .{
            .type_ = result_type,
            .kind = .{ .UNARY_OP = .{ .op = op, .operand = operand_expr } },
        });
        return expr_idx;
    }

    fn inferCallOrInst(ti: *TypeInferer, ast_idx: AstNodeIndex) !ExpressionIndex {
        const node = ti.ast.get(ast_idx);
        assert(node.tag == .CALL_OR_INST);

        const decl_idx = ti.di.name_resolution[ast_idx];
        if (decl_idx == .NONE) {
            // Unknown — error was already reported in name resolution
            const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
            try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
            return expr_idx;
        }

        const decl = ti.di.declarations.items[@intFromEnum(decl_idx)];

        switch (decl.kind) {
            .FUNCTION => return ti.inferFnCall(ast_idx, decl_idx),
            .STRUCT => return ti.inferStructInst(ast_idx, decl_idx),
            else => {
                // symbol is not callable — error was already reported in name resolution
                const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
                try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
                return expr_idx;
            },
        }
    }

    const ResolvedArgs = struct {
        types: [MAX_NUM_FUNCTION_PARAMS]DkType,
        exprs: [MAX_NUM_FUNCTION_PARAMS]ExpressionIndex,
        count: u8,
    };

    fn resolveArgs(ti: *TypeInferer, ast_idx: AstNodeIndex, params: []const nr.ParamOrMember, param_count: u8) !ResolvedArgs {
        var result = ResolvedArgs{
            .types = .{DkType.UNKNOWN} ** MAX_NUM_FUNCTION_PARAMS,
            .exprs = .{0} ** MAX_NUM_FUNCTION_PARAMS,
            .count = param_count,
        };
        var param_set: [MAX_NUM_FUNCTION_PARAMS]bool = .{false} ** MAX_NUM_FUNCTION_PARAMS;
        var positional_count: u8 = 0;

        const node = ti.ast.get(ast_idx);
        var child_iter = node.children(ti.ast);
        while (child_iter.nextIdx()) |arg_ast_idx| {
            const arg_node = ti.ast.get(arg_ast_idx);

            if (arg_node.tag == .NAMED_ARG) {
                const arg_name = ti.tokens[arg_node.token_index].str(ti.source);
                var found_idx: ?u8 = null;
                for (params, 0..) |p, i| {
                    if (std.mem.eql(u8, ti.tokens[p.name_token_idx].str(ti.source), arg_name)) {
                        found_idx = @intCast(i);
                        break;
                    }
                }

                if (found_idx == null) {
                    try ti.addError(.{ .unknown_named_arg = .{ .ast_node = arg_ast_idx, .member_name = arg_node.token_index } });
                    continue;
                }

                const mi = found_idx.?;
                if (param_set[mi]) {
                    try ti.addError(.{ .duplicate_named_arg = .{ .ast_node = arg_ast_idx, .member_name = arg_node.token_index } });
                    continue;
                }

                const value_expr = try ti.inferExpr(arg_node.first_child);
                result.types[mi] = ti.ft.expressions.items[value_expr].type_;
                result.exprs[mi] = value_expr;
                param_set[mi] = true;
            } else {
                if (positional_count >= param_count) {
                    try ti.addError(.{ .too_many_positional_args = .{ .ast_node = ast_idx, .expected = param_count, .actual = positional_count + 1 } });
                    break;
                }
                const value_expr = try ti.inferExpr(arg_ast_idx);
                result.types[positional_count] = ti.ft.expressions.items[value_expr].type_;
                result.exprs[positional_count] = value_expr;
                param_set[positional_count] = true;
                positional_count += 1;
            }
        }

        for (params, 0..) |p, i| {
            if (param_set[i]) continue;
            if (p.default_ast_idx != 0) {
                const default_expr = try ti.inferExpr(p.default_ast_idx);
                result.types[i] = ti.ft.expressions.items[default_expr].type_;
                result.exprs[i] = default_expr;
                param_set[i] = true;
            } else {
                try ti.addError(.{ .missing_required_arg = .{ .ast_node = ast_idx, .member_name = p.name_token_idx } });
            }
        }

        return result;
    }

    fn inferFnCall(ti: *TypeInferer, ast_idx: AstNodeIndex, decl_idx: DeclIndex) !ExpressionIndex {
        // Find function template by decl_idx
        const ft_idx = blk: {
            for (ti.di.fn_templates.items[1..], 1..) |ft_item, i| {
                if (ft_item.decl_idx == decl_idx)
                    break :blk @as(nr.FunctionTemplates.Index, @enumFromInt(i));
            }
            unreachable;
        };

        const fn_template = &ti.di.fn_templates.items[@intFromEnum(ft_idx)];
        const params = fn_template.params(&ti.di.params_or_members);

        const resolved = try ti.resolveArgs(ast_idx, params, fn_template.param_count);
        const arg_types = resolved.types[0..fn_template.param_count];

        // Build linked list of arg expressions in param order (includes evaluated defaults for omitted args)
        var first_arg_expr: ExpressionIndex = 0;
        var prev_arg_expr: ExpressionIndex = 0;
        for (0..fn_template.param_count) |i| {
            const ae = resolved.exprs[i];
            if (ae == 0) continue;
            if (first_arg_expr == 0)
                first_arg_expr = ae
            else
                ti.ft.expressions.items[prev_arg_expr].next_sibling = ae;
            prev_arg_expr = ae;
        }

        // Instantiate function (does type checking and creates FnDecl if needed)
        const return_type = ti.instantiateFunction(ast_idx, ft_idx, arg_types) catch |err| switch (err) {
            error.WrongNumberOfFunctionArguments => {
                const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
                try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
                return expr_idx;
            },
            error.OutOfMemory => return error.OutOfMemory,
        };

        // Find the FnDeclIndex for this instance
        const inst = ti.fn_instances.getPtr(.{ .template_idx = ft_idx, .param_types = arg_types }).?;
        const fn_decl_idx = inst.fn_decl_idx;

        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
        try ti.ft.expressions.append(ti.gpa, .{
            .type_ = return_type,
            .kind = .{ .FN_CALL = .{ .function = fn_decl_idx, .args_start = first_arg_expr } },
        });
        return expr_idx;
    }

    fn inferStructInst(ti: *TypeInferer, ast_idx: AstNodeIndex, decl_idx: DeclIndex) !ExpressionIndex {
        // Find struct template by decl_idx
        const st_idx = blk: {
            for (ti.di.struct_templates.items[1..], 1..) |st_item, i| {
                if (st_item.decl_idx == decl_idx)
                    break :blk @as(nr.StructTemplates.Index, @enumFromInt(i));
            }
            unreachable;
        };

        const template = &ti.di.struct_templates.items[@intFromEnum(st_idx)];
        const members = template.members(&ti.di.params_or_members);

        const resolved = try ti.resolveArgs(ast_idx, members, template.member_count);

        // Instantiate struct type
        const struct_type = try ti.instantiateStruct(st_idx, resolved.types[0..template.member_count]);

        // Build linked list of member value expressions in member order
        var first_member_expr: ExpressionIndex = 0;
        var prev_member_expr: ExpressionIndex = 0;
        for (0..template.member_count) |i| {
            const me = resolved.exprs[i];
            if (me == 0) continue;
            if (first_member_expr == 0)
                first_member_expr = me
            else
                ti.ft.expressions.items[prev_member_expr].next_sibling = me;
            prev_member_expr = me;
        }

        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
        try ti.ft.expressions.append(ti.gpa, .{
            .type_ = struct_type,
            .kind = .{ .STRUCT_INST = .{
                .struct_instance = struct_type.structInstanceIdx(),
                .args_start = first_member_expr,
            } },
        });
        return expr_idx;
    }

    fn inferMemberAccess(ti: *TypeInferer, ast_idx: AstNodeIndex) !ExpressionIndex {
        const node = ti.ast.get(ast_idx);
        assert(node.tag == .MEMBER_ACCESS);

        // Infer base expression
        const base_expr = try ti.inferExpr(node.first_child);
        const base_type = ti.ft.expressions.items[base_expr].type_;

        if (base_type == DkType.ERROR) {
            const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
            try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
            return expr_idx;
        }

        if (!base_type.isStruct()) {
            try ti.addError(.{ .member_access_on_non_struct = .{ .ast_node = ast_idx, .actual = base_type } });
            const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
            try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
            return expr_idx;
        }

        const si = ti.struct_instances.items[base_type.structInstanceIdx()];
        const template = &ti.di.struct_templates.items[@intFromEnum(si.template_idx)];
        const members = template.members(&ti.di.params_or_members);
        const field_name = ti.tokens[node.token_index].str(ti.source);
        const member_concrete_types = ti.types.slice(si.first_member_type_idx, si.member_count);

        var member_idx: ?u8 = null;
        for (members, 0..) |m, i| {
            if (std.mem.eql(u8, ti.tokens[m.name_token_idx].str(ti.source), field_name)) {
                member_idx = @intCast(i);
                break;
            }
        }

        if (member_idx == null) {
            try ti.addError(.{ .unknown_field = .{ .ast_node = ast_idx, .field_name = node.token_index } });
            const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
            try ti.ft.expressions.append(ti.gpa, .{ .type_ = DkType.ERROR });
            return expr_idx;
        }

        const member_type = member_concrete_types[member_idx.?];

        const expr_idx: ExpressionIndex = @intCast(ti.ft.expressions.items.len);
        try ti.ft.expressions.append(ti.gpa, .{
            .type_ = member_type,
            .kind = .{ .MEMBER_ACCESS = .{ .base = base_expr, .member_idx = member_idx.? } },
        });
        return expr_idx;
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    fn tokenToAssignmentKind(tag: tok.Token.Tag) AssignmentKind {
        return switch (tag) {
            // zig fmt: off
            .ASSIGN      => .ASSIGN,
            .PLUSASSIGN   => .PLUS,
            .MINUSASSIGN => .MINUS,
            .MULTASSIGN  => .MULT,
            .DIVASSIGN   => .DIV,
            // zig fmt: on
            else => unreachable,
        };
    }
};

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

test "merged: zinseszins program" {
    const source =
        \\fn do_zins(prev_val, zins)
        \\    result = prev_val * zins
        \\
        \\fn zinseszins(initial, zins, jahre)
        \\      result = initial
        \\      jahr := 0
        \\      while jahr < jahre
        \\         result = do_zins(result, zins)
        \\         jahr += 1
        \\
        \\fn main()
        \\    jahr := 0
        \\    zins := 1.02
        \\    result = zinseszins(1000.0, zins, 10)
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    // Should have 4 fn_decls: [0]=invalid, [1]=do_zins(FLOAT,FLOAT), [2]=zinseszins(FLOAT,FLOAT,INT), [3]=main()
    // do_zins is also called with INT args from the lowered code? No — zinseszins passes result (FLOAT) and zins (FLOAT).
    // Actually: do_zins is called once with (FLOAT, FLOAT). zinseszins is called once with (FLOAT, FLOAT, INT).
    try std.testing.expectEqual(@as(usize, 4), ft.fn_decls.items.len);

    // Each function should have a valid body scope
    for (ft.fn_decls.items[1..]) |fn_decl|
        try std.testing.expect(fn_decl.body_scope != 0);

    // Check return types
    try std.testing.expectEqual(DkType.FLOAT, ft.fn_decls.items[1].return_type); // do_zins returns FLOAT
    try std.testing.expectEqual(DkType.FLOAT, ft.fn_decls.items[2].return_type); // zinseszins returns FLOAT
    try std.testing.expectEqual(DkType.FLOAT, ft.fn_decls.items[3].return_type); // main returns FLOAT
}

test "merged: type error in binary op" {
    const source =
        \\fn main()
        \\    x := 1
        \\    y := true
        \\    result = x + y
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(ft.hasErrors());
    // x + y where x:INT, y:BOOL → "RHS must be of number type"
    try std.testing.expectEqual(@as(usize, 1), ft.errors.items.len);
    try std.testing.expect(ft.errors.items[0].error_ == .wrong_type);
}

test "merged: generic function instantiation" {
    const source =
        \\fn add(a, b)
        \\    result = a + b
        \\
        \\fn main()
        \\    x := add(1, 2)
        \\    y := add(1.0, 2.0)
        \\    result = x + y
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    // add(INT,INT) returns INT, add(FLOAT,FLOAT) returns FLOAT, x + y is type mismatch (INT + FLOAT)
    try std.testing.expect(ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.errors.items.len);
    try std.testing.expect(ft.errors.items[0].error_ == .type_mismatch);
    try std.testing.expectEqual(DkType.INT, ft.errors.items[0].error_.type_mismatch.lhs);
    try std.testing.expectEqual(DkType.FLOAT, ft.errors.items[0].error_.type_mismatch.rhs);

    // Should have 4 fn_decls: invalid + add(INT,INT) + add(FLOAT,FLOAT) + main
    try std.testing.expectEqual(@as(usize, 4), ft.fn_decls.items.len);
}

test "function with default args" {
    const source =
        \\fn greet(a, b := 10)
        \\    result = a + b
        \\
        \\fn main()
        \\    x := greet(1)
        \\    y := greet(2, 3)
        \\    result = x + y
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(!ft.hasErrors());
    // greet(INT,INT) monomorphized once, main
    try std.testing.expectEqual(@as(usize, 3), ft.fn_decls.items.len);
}

test "function with named args" {
    const source =
        \\fn calc(a, b)
        \\    result = a - b
        \\
        \\fn main()
        \\    result = calc(b=1, a=5)
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(!ft.hasErrors());
}

test "function missing required arg" {
    const source =
        \\fn calc(a, b)
        \\    result = a + b
        \\
        \\fn main()
        \\    result = calc(1)
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .missing_required_arg);
}

test "non-default param after default" {
    const source =
        \\fn bad(a := 1, b)
        \\    result = a + b
        \\
        \\fn main()
        \\    result = bad(1, 2)
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();

    try std.testing.expect(di.hasErrors());
    var found = false;
    for (di.errors.items) |e| {
        if (e == .non_default_param_after_default)
            found = true;
    }
    try std.testing.expect(found);
}

test "supply wrong type to fn param with default" {
    const source =
        \\fn bad(a, b:=1)
        \\    result = a + b
        \\
        \\fn main()
        \\    result = bad(1, true)
    ;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(ft.hasErrors());
    var err : ?TypeErrorInfo = null;
    for (ft.errors.items) |e| {
        if (e.error_ == .wrong_type)
            err = e;
    }

    try std.testing.expect(err != null);
    try std.testing.expect(err.?.error_.wrong_type.actual == DkType.BOOL); // error is on the call site
}

// -----------------------------------------------------------------------
// arrayShapeFromLit unit tests
// -----------------------------------------------------------------------

fn parseExprForShapeTest(source: []const u8, gpa: std.mem.Allocator) !struct { ast: par.AST, root: par.AstNodeIndex } {
    var ts = try tok.TokenStream.init(source, gpa);
    defer ts.deinit(gpa);
    var p = try par.Parser.init(source, ts.tokens, gpa);
    const root = try p.parseExpression();
    return .{ .ast = p.ast, .root = root };
}

test "arrayShapeFromLit: flat [1,2,3]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[1,2,3]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 1), shape.ndim);
    try std.testing.expectEqual(@as(u16, 3), shape.dims[0].count);
    try std.testing.expect(!shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: fill in middle [1,0...,2]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[1,0...,2]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 1), shape.ndim);
    try std.testing.expectEqual(@as(u16, 2), shape.dims[0].count);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: only fill [0...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[0...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 1), shape.ndim);
    try std.testing.expectEqual(@as(u16, 0), shape.dims[0].count);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: 2D [[1,2],[3,4]]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[[1,2],[3,4]]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 2), shape.ndim);
    try std.testing.expectEqual(@as(u16, 2), shape.dims[0].count);
    try std.testing.expect(!shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(u16, 2), shape.dims[1].count);
    try std.testing.expect(!shape.dims[1].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: 2D with fill row [[1,2,3],[0...]...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[[1,2,3],[0...]...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 2), shape.ndim);
    try std.testing.expectEqual(@as(u16, 1), shape.dims[0].count);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(u16, 3), shape.dims[1].count);
    try std.testing.expect(!shape.dims[1].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: 2D all fills [[0...]...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[[0...]...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 2), shape.ndim);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expect(shape.dims[1].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: multiple fills same level [1,0...,2,0...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[1,0...,2,0...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expect(shape.extra_fill_node != 0);
}

// -----------------------------------------------------------------------
// Full pipeline array type inference tests
// -----------------------------------------------------------------------

fn inferProgram(source: []const u8, gpa: std.mem.Allocator) !ft_ast.FtAst {
    var ts = try tok.TokenStream.init(source, gpa);
    defer ts.deinit(gpa);
    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    return infer(gpa, ts, &pr.ast, &di);
}

test "array: annotated [3]Int = [1,2,3]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,2,3]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.array_instances.items.len);
    const ai = ft.array_instances.items[0];
    try std.testing.expectEqual(@as(u8, 1), ai.ndim);
    try std.testing.expectEqual(@as(u16, 3), ai.shape[0]);
    try std.testing.expectEqual(DkType.INT, ai.elem_type);
    // var decl should have array type
    const main_scope = ft.fn_decls.items[1].body_scope;
    const var_decl_idx = ft.scopes.items[main_scope].first_decl;
    try std.testing.expect(ft.var_decls.items[var_decl_idx].type_.isArray());
}

test "array: inferred := [1,2,3]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a := [1,2,3]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.array_instances.items.len);
    const ai = ft.array_instances.items[0];
    try std.testing.expectEqual(@as(u8, 1), ai.ndim);
    try std.testing.expectEqual(@as(u16, 3), ai.shape[0]);
    try std.testing.expectEqual(DkType.INT, ai.elem_type);
}

test "array: 2D annotated [2,3]Int" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [2,3]Int = [[1,2,3],[4,5,6]]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    // expect 2 array instances: [3]Int (inner) and [2,3]Int (outer)
    var found_outer = false;
    for (ft.array_instances.items) |ai| {
        if (ai.ndim == 2 and ai.shape[0] == 2 and ai.shape[1] == 3 and ai.elem_type == DkType.INT)
            found_outer = true;
    }
    try std.testing.expect(found_outer);
}

test "array: fill with annotation [4]Int = [1,0...,2]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [4]Int = [1,0...,2]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.array_instances.items.len);
    try std.testing.expectEqual(@as(u16, 4), ft.array_instances.items[0].shape[0]);
}

test "array: shape mismatch [3]Int = [1,2]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,2]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .array_shape_mismatch);
}

test "array: cannot infer size with fill" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a := [1,0...]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .cannot_infer_array_size);
}

test "array: multiple fills same level" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,0...,2,0...]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    var found = false;
    for (ft.errors.items) |e| if (e.error_ == .multiple_fills_in_array_literal) { found = true; break; };
    try std.testing.expect(found);
}

test "array: inconsistent elem types [1,2,true]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,2,true]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    var found = false;
    for (ft.errors.items) |e| if (e.error_ == .inconsistent_elem_types_in_array_lit) { found = true; break; };
    try std.testing.expect(found);
}

test "array: ndim mismatch [3]Int = [[1,2,3]]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [[1,2,3]]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .array_shape_mismatch);
}

test "array: inconsistent inner dim [[1,2],[1,2,3]]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [2,3]Int = [[1,2],[1,2,3]]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    var found = false;
    for (ft.errors.items) |e| if (e.error_ == .inconsistent_inner_dim) { found = true; break; };
    try std.testing.expect(found);
}
