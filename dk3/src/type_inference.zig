const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const util = @import("util.zig");
const ft_ast = @import("ft_ast.zig");


const assert = std.debug.assert;

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;
const SourceLoc = tok.SourceLoc;

const AST = par.AST;
const AstNode = par.AstNode;
const Parser = par.Parser;
const AstNodeIndex = par.AstNodeIndex;

const FtAst = ft_ast.FtAst;

const MAX_NUM_FUNCTION_PARAMS = 32;

const builtin_functions = [_]FunctionInstance{
    .{ .name = .builtin("print"), .return_type = .VOID, .param_count = 1, .first_param_type_idx = .NONE }, // FIXME: set propper type for param
};

pub const DkType = enum(u8) {
    UNKNOWN = 0,
    VOID,
    BOOL,
    INT,
    FLOAT,
    ANY,
    ERROR,
};

const SymbolInfo = struct {
    declaration: TokenIndex,
    kind: union(enum) {
        variable: DkType,
        function: FunctionTemplates.Index,
        fn_param: DkType,
        result_variable: DkType,
    },
};

const SymbolTable = std.hash_map.StringHashMapUnmanaged(SymbolInfo);

const SymbolTablePool = struct {
    pool: std.ArrayList(SymbolTable),
    gpa: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator, initial_capacity: usize) !SymbolTablePool {
        var res = SymbolTablePool{
            .pool = try .initCapacity(gpa, initial_capacity),
            .gpa = gpa,
        };

        for (0..initial_capacity) |_|
            res.pool.appendAssumeCapacity(.{});

        return res;
    }

    pub fn deinit(self: *SymbolTablePool) void {
        for (self.pool.items) |*table|
            table.deinit(self.gpa);
        self.pool.deinit(self.gpa);
    }

    pub fn get(self: *SymbolTablePool) !SymbolTable {
        if (self.pool.items.len == 0)
            try self.pool.append(self.gpa, .{});

        self.pool.items.len -= 1;
        return self.pool.items.ptr[self.pool.items.len];
    }

    pub fn release(self: *SymbolTablePool, table: *SymbolTable) void {
        table.clearRetainingCapacity();
        assert(self.pool.items.len < self.pool.capacity);
        self.pool.appendAssumeCapacity(table.*);
    }

    pub fn releaseAll(self: *SymbolTablePool, tables: []SymbolTable) void {
        for (tables) |*table|
            self.release(table);
    }
};

const UndeclaredSymbolTable = std.hash_map.StringHashMapUnmanaged(TokenIndex);

const SymbolTableStack = struct {
    pool: *SymbolTablePool,

    // Each of the following stacks has 1 entry per block nesting level.
    // I.e. at index 0 are the declared(/undeclared) identefiers of the root block.
    // An immediate child block of the root block will be at index 1, its children at 2, etc.
    tables: [MAX_NESTING_LEVEL]SymbolTable,
    undeclared: [MAX_NESTING_LEVEL]UndeclaredSymbolTable = [1]UndeclaredSymbolTable{.{}} ** MAX_NESTING_LEVEL, // only used in case of error. no pooling
    nesting_level: usize = 0,

    const MAX_NESTING_LEVEL = 16;

    pub fn init(pool: *SymbolTablePool) !SymbolTableStack {
        var res = SymbolTableStack{
            .pool = pool,
            .tables = undefined,
        };
        res.tables[0] = try pool.get();

        return res;
    }

    pub fn deinit(self: *SymbolTableStack) void {
        self.pool.releaseAll(self.tables[0 .. self.nesting_level + 1]);
        for (0..MAX_NESTING_LEVEL) |i| {
            self.undeclared[i].deinit(self.pool.gpa);
        }
    }

    pub fn getMutablePtr(self: *SymbolTableStack, name: []const u8) ?*SymbolInfo {
        for (0..self.nesting_level + 1) |i| {
            if (self.tables[i].getPtr(name)) |info|
                return info;
        }
        return null;
    }

    pub fn getPtr(self: *const SymbolTableStack, name: []const u8) ?*const SymbolInfo {
        return @constCast(self).getMutablePtr(name);
    }

    pub fn enterBlock(self: *SymbolTableStack) !void {
        self.nesting_level += 1;
        if (self.nesting_level >= MAX_NESTING_LEVEL)
            return error.NestingLevelExceedsMaximum;
        self.tables[self.nesting_level] = try self.pool.get();
    }

    pub fn exitBlock(self: *SymbolTableStack) void {
        assert(self.nesting_level > 0);
        self.pool.release(&self.tables[self.nesting_level]);
        self.undeclared[self.nesting_level].clearRetainingCapacity();
        self.nesting_level -= 1;
    }
};

pub const TypeInferer = struct {
    source: []const u8,
    tokens: []const Token,
    ts : TokenStream,
    ast: *const AST,
    // node_types: []DkType, // types for each node in the ast
    name_resolution: []TokenIndex, // indexed by AstNodeIndex. 0 = not annotated. Maps identifier usages to their declaration token index.
    root_index: AstNodeIndex,
    gpa: std.mem.Allocator,

    // for error reporting:
    fn_instantiation_stack: std.ArrayList(FunctionInstances.Index) = .empty,
    fn_instantiation_ast_idx_stack: std.ArrayList(AstNodeIndex) = .empty, 

    // types: Types,
    finfo: FunctionInfos,
    symbol_table_pool: SymbolTablePool,

    global_symbol_table: SymbolTable = .{}, // used for global variables and function declarations. never popped.

    // declarations: [MAX_NESTING_LEVEL]std.hash_map.StringHashMapUnmanaged(VariableInfo), // used as a stack
    // declarations: [MAX_NESTING_LEVEL]SymbolTable, // used as a stack
    // undeclared__: [MAX_NESTING_LEVEL]std.hash_map.StringHashMapUnmanaged(TokenIndex), // used as a stack
    // nesting_level: usize = 0,
    // declared_variables: std.hash_map.StringHashMapUnmanaged(TokenIndex),
    // undeclared_variables: std.hash_map.StringHashMapUnmanaged(TokenIndex),
    errors: std.ArrayList(ErrorInfo) = .empty,

    const MAX_NESTING_LEVEL = 16;

    pub const ErrorInfo = struct {
        error_: Error,
        fn_instantiation_stack_top: [16]InstantiationInfo, // in reversed order! i.e. the current function instantiation is at index 0, its caller at index 1, etc.
        fn_instantiation_stack_len: usize,

        const InstantiationInfo = struct {
            function: FunctionInstances.Index,
            fn_call_ast_idx: AstNodeIndex,
        };

        const MAX_FN_INSANTIATION_REPORT_DEPTH = 16;
    };

    pub const Error = union(enum) {
        undecl_var: TokenIndex,
        multi_decl_var: struct {
            first_decl: TokenIndex,
            error_decl: TokenIndex,
        },
        multi_decl_fn: struct {
            first_decl: TokenIndex,
            error_decl: TokenIndex,
        },
        decl_shadows_outer: struct {
            outer_decl: TokenIndex,
            error_decl: TokenIndex,
        },
        unknown_function: AstNodeIndex,

        symbol_is_not_a_function: struct {
            declaration: TokenIndex,
            call: TokenIndex,
        },
        symbol_is_not_a_variable_but_function: struct {
            declaration: TokenIndex,
            usage: TokenIndex,
        },

        fn_params_are_immutable: struct {
            declaration: TokenIndex,
            usage: TokenIndex,
        },

        wrong_num_fun_args: struct {
            ast_node: AstNodeIndex,
            expected: u8,
            actual: u8,
        },

        redeclaration_of_result_var: TokenIndex,

        param_shadows_global: struct {
            global_decl: TokenIndex,
            param_decl: TokenIndex,
        },

        function_parameters_have_same_name: struct {
            first_param: TokenIndex,
            second_param: TokenIndex,
        },

        // typing errors

        return_type_missing_for_recursive_fn: AstNodeIndex, // AST index of function head

        wrong_type: struct {
            ast_node: AstNodeIndex,
            expected: [:0]const u8,
            actual: DkType,
        },
        type_mismatch: struct {
            ast_node: AstNodeIndex,
            lhs: DkType,
            rhs: DkType,
        },
        result_type_mismatch: struct {
            ast_node: AstNodeIndex,
            lhs: DkType,
            rhs: DkType,
        },
    };

    pub const TypeInfererException = error{
        OutOfMemory,
        WrongNumberOfFunctionArguments,
        NestingLevelExceedsMaximum,
    };

    pub fn init(
        gpa: std.mem.Allocator,
        ts : TokenStream,
        parser: *const Parser,
    ) !TypeInferer {
        const name_resolution = try gpa.alloc(TokenIndex, parser.ast.nodeCount());
        @memset(name_resolution, 0);

        var ti = TypeInferer{
            .source = parser.source,
            .tokens = parser.tokens,
            .ast = &parser.ast,
            .ts = ts,
            .name_resolution = name_resolution,
            .root_index = parser.root_node,
            .gpa = gpa,
            .finfo = undefined,
            .symbol_table_pool = try SymbolTablePool.init(gpa, 64),
        };

        ti.finfo = try FunctionInfos.init(gpa);
        try ti.fn_instantiation_stack.ensureTotalCapacity(gpa, 64);
        // for(ti.node_types) |*nt|
        //     nt.* = .UNKNOWN;

        return ti;
    }

    pub fn deinit(ti: *TypeInferer) void {
        ti.finfo.deinit();
        ti.errors.deinit(ti.gpa);
        ti.global_symbol_table.deinit(ti.gpa);
        ti.symbol_table_pool.deinit();
        ti.fn_instantiation_stack.deinit(ti.gpa);
        ti.fn_instantiation_ast_idx_stack.deinit(ti.gpa);
        ti.gpa.free(ti.name_resolution);
    }

    pub fn hasErrors(ti: *const TypeInferer) bool {
        return ti.errors.items.len > 0;
    }

    fn addError(ti: *TypeInferer, error_: Error) !void {
        // const stack_depth = ti.fn_instantiation_stack.items.len;
        // const start = if(stack_depth <= ErrorInfo.MAX_FN_INSANTIATION_REPORT_DEPTH) 0 else (stack_depth - ErrorInfo.MAX_FN_INSANTIATION_REPORT_DEPTH);
        const len = @min(ti.fn_instantiation_stack.items.len, ErrorInfo.MAX_FN_INSANTIATION_REPORT_DEPTH);
        var error_info = ErrorInfo{
            .error_ = error_,
            .fn_instantiation_stack_top = undefined,
            .fn_instantiation_stack_len = len,
        };
        for (0..len) |i| {
            const idx1  = @as(i32, @intCast(ti.fn_instantiation_stack.items.len        )) - @as(i32, @intCast(1 + i));
            const idx2  = @as(i32, @intCast(ti.fn_instantiation_ast_idx_stack.items.len)) - @as(i32, @intCast(i));

            error_info.fn_instantiation_stack_top[i] = .{ 
                .function = ti.fn_instantiation_stack.items[@intCast(idx1)], 
                .fn_call_ast_idx = if(idx2 <  ti.fn_instantiation_ast_idx_stack.items.len) ti.fn_instantiation_ast_idx_stack.items[@intCast(idx2)] else 0,
            };
        }

        try ti.errors.append(ti.gpa, error_info);
    }

    fn typecheckSameType(ti: *TypeInferer, op_index: AstNodeIndex, lhs: AstNodeIndex, rhs: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const lhs_type = try ti.inferType(lhs, symbol_table_stack);
        const rhs_type = try ti.inferType(rhs, symbol_table_stack);

        if (lhs_type == .ERROR or rhs_type == .ERROR)
            return .ERROR; // this error is a folowup error so we do not report it.

        if (lhs_type != rhs_type or lhs_type == .UNKNOWN) {
            try ti.addError(.{ .type_mismatch = .{ .ast_node = op_index, .lhs = lhs_type, .rhs = rhs_type } });
            return .ERROR;
        }

        return .BOOL;
    }

    fn inferArithmeticBinaryOp(ti: *TypeInferer, op_idx: AstNodeIndex, lhs: AstNodeIndex, rhs: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const lhs_type = try ti.inferType(lhs, symbol_table_stack);
        const rhs_type = try ti.inferType(rhs, symbol_table_stack);

        if (lhs_type == .ERROR or rhs_type == .ERROR)
            return .ERROR; // this error is a folowup error so we do not report it.

        var err_occured = false;
        if (lhs_type != .INT and lhs_type != .FLOAT) {
            try ti.addError(.{ .wrong_type = .{ .ast_node = lhs, .actual = lhs_type, .expected = "LHS must be of number type" } });
            err_occured = true;
        }
        if (rhs_type != .INT and rhs_type != .FLOAT) {
            try ti.addError(.{ .wrong_type = .{ .ast_node = rhs, .actual = rhs_type, .expected = "RHS must be of number type" } });
            err_occured = true;
        }

        if (err_occured)
            return .ERROR;

        if (lhs_type != rhs_type or lhs_type == .UNKNOWN) {
            try ti.addError(.{ .type_mismatch = .{ .ast_node = op_idx, .lhs = lhs_type, .rhs = rhs_type } });
            return .ERROR;
        }

        return lhs_type;
    }

    fn inferBooleanBinaryOp(ti: *TypeInferer, lhs: AstNodeIndex, rhs: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const lhs_type = try ti.inferType(lhs, symbol_table_stack);
        const rhs_type = try ti.inferType(rhs, symbol_table_stack);

        if (lhs_type == .ERROR or rhs_type == .ERROR)
            return .ERROR; // this error is a folowup error so we do not report it.

        var err_occured = false;
        if (lhs_type != .BOOL) {
            try ti.addError(.{ .wrong_type = .{ .ast_node = lhs, .actual = lhs_type, .expected = "LHS must be a boolean" } });
            err_occured = true;
        }
        if (rhs_type != .BOOL) {
            try ti.addError(.{ .wrong_type = .{ .ast_node = rhs, .actual = rhs_type, .expected = "RHS must be a boolean" } });
            err_occured = true;
        }

        if (err_occured)
            return .ERROR;

        return .BOOL;
    }

    fn inferBinaryOp(ti: *TypeInferer, op_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(op_idx);
        const token = ti.tokens[node.token_index];
        const lhs = node.first_child;
        const rhs = ti.ast.get(lhs).next_sibling;
        assert(lhs != 0);
        assert(rhs != 0);
        switch (token.tag) {
            .EQ, .NOT_EQ, .LT, .LE, .GT, .GE => return ti.typecheckSameType(op_idx, lhs, rhs, symbol_table_stack),
            .PLUS, .MINUS, .DIV, .TIMES, .POW => return ti.inferArithmeticBinaryOp(op_idx, lhs, rhs, symbol_table_stack),
            .AND, .OR, .XOR => return ti.inferBooleanBinaryOp(lhs, rhs, symbol_table_stack),
            else => {
                std.debug.print("#### INTERNAL ERROR: node: {any} token: {any} ##########\n\n", .{ node, token });
                unreachable;
            },
        }
    }

    fn inferUnaryOp(ti: *TypeInferer, op_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(op_idx);
        const token = ti.tokens[node.token_index];
        const child_idx = node.first_child;
        const child_type = try ti.inferType(child_idx, symbol_table_stack);
        switch (token.tag) {
            .NOT => switch (child_type) {
                .ERROR => return .ERROR,
                .BOOL => return .BOOL,
                else => {
                    try ti.addError(.{ .wrong_type = .{ .ast_node = child_idx, .actual = child_type, .expected = "Child expression must be a boolean" } });
                    return .ERROR;
                },
            },
            .MINUS => switch (child_type) {
                .ERROR => return .ERROR,
                .INT, .FLOAT => return child_type,
                else => {
                    try ti.addError(.{ .wrong_type = .{ .ast_node = child_idx, .actual = child_type, .expected = "Child expression must be a number" } });
                    return .ERROR;
                },
            },
            else => unreachable,
        }
    }

    fn expectType(ti: *TypeInferer, node_idx: AstNodeIndex, actual: DkType, comptime expected: DkType) !void {
        if (actual == .ERROR)
            return; // this error is a folowup error so we do not report it.    

        if (actual != expected)
            try ti.addError(.{ .wrong_type = .{ .ast_node = node_idx, .actual = actual, .expected = "must be of type " ++ @tagName(expected) } });

        return;
    }

    fn inferDecl(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .DECLARATION);

        // get LHS:
        assert(node.first_child != 0);
        const var_node = ti.ast.get(node.first_child);
        const var_token_index = var_node.token_index;
        const var_token = ti.tokens[var_token_index];

        assert(var_token.tag == .IDENTIFIER);

        // typecheck RHS:
        const rhs_type = try ti.inferType(var_node.next_sibling, symbol_table_stack);

        try ti.registerDeclaration(var_token_index, rhs_type, symbol_table_stack);

        // ti.node_types[node.first_child] = rhs_type;
        return rhs_type;
    }

    fn inferAssignment(ti: *TypeInferer, op_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(op_idx);
        const op_token = ti.tokens[node.token_index];

        const lhs_idx = node.first_child;
        assert(lhs_idx > 0);

        // get LHS:
        const var_node = ti.ast.get(lhs_idx);
        const var_token_index = var_node.token_index;
        const var_token = ti.tokens[var_token_index];
        assert(var_token.tag == .IDENTIFIER);
        const varname = var_token.str(ti.source);

        const rhs_idx = var_node.next_sibling;
        assert(rhs_idx > 0);

        const symbol = symbol_table_stack.getMutablePtr(varname) orelse {
            try ti.addError(.{ .undecl_var = var_token_index });
            return .ERROR;
        };

        switch (symbol.kind) {
            .fn_param => {
                try ti.addError(.{ .fn_params_are_immutable = .{ .declaration = symbol.declaration, .usage = var_token_index } });
                return .ERROR;
            },
            .variable => {
                const rhs_type = switch (op_token.tag) {
                    // zig fmt: off
                    .PLUSASSIGN,
                    .MINUSASSIGN,
                    .MULTASSIGN,
                    .DIVASSIGN    => try ti.inferArithmeticBinaryOp(op_idx, lhs_idx, rhs_idx, symbol_table_stack),
                    .ASSIGN       => try ti.typecheckSameType(op_idx ,lhs_idx, rhs_idx, symbol_table_stack),
                    else          => unreachable,
                    // zig fmt: on
                };

                // ti.node_types[node.first_child] = rhs_type;
                return rhs_type;
            },
            .result_variable => |result_type| {
                ti.name_resolution[lhs_idx] = symbol.declaration;
                const rhs_type = try ti.inferType(rhs_idx, symbol_table_stack);
                if (result_type == .UNKNOWN) {
                    // this is the first assignment to the result variable, so we can set its type now:
                    symbol.kind = .{ .result_variable = rhs_type };
                } else if (result_type != rhs_type and rhs_type != .ERROR) {
                    try ti.addError(.{ .result_type_mismatch = .{ .ast_node = op_idx, .lhs = result_type, .rhs = rhs_type } });
                    return .ERROR;
                }
                return rhs_type;
            },
            .function => |fn_template_idx| {
                assert(fn_template_idx != .NONE);
                const fn_template = ti.finfo.templates.get(fn_template_idx);
                const token_idx = ti.ast.get(fn_template.ast_idx).token_index;
                try ti.addError(.{ .symbol_is_not_a_variable_but_function = .{ .declaration = token_idx, .usage = lhs_idx } });
                return .ERROR;
            },
        }
    }

    fn inferAtom(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .ATOM);
        const token = ti.tokens[node.token_index];
        return switch (token.tag) {
            // zig fmt: off
            .TRUE, .FALSE => .BOOL,
            .INT_LIT      => .INT,
            .FLOAT_LIT    => .FLOAT,
            .IDENTIFIER   => ti.getTypeOfVariable(node_idx, node.token_index, symbol_table_stack),
            // zig fmt: on
            else => unreachable,
        };
    }

    fn getTypeOfVariable(ti: *TypeInferer, node_idx: AstNodeIndex, identifier_index: TokenIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const token = ti.tokens[identifier_index];
        std.debug.assert(token.tag == .IDENTIFIER);
        const varname = token.str(ti.source);

        for (0..symbol_table_stack.nesting_level + 1) |nl| {
            if (symbol_table_stack.tables[nl].get(varname)) |var_info| {
                switch(var_info.kind) {
                    .variable, .fn_param, .result_variable => |t| {
                        ti.name_resolution[node_idx] = var_info.declaration;
                        return t;
                    },
                    .function => |fn_template_idx| {
                        assert(fn_template_idx != .NONE);
                        const fn_template = ti.finfo.templates.get(fn_template_idx);
                        const token_idx = ti.ast.get(fn_template.ast_idx).token_index;
                        try ti.addError(.{ .symbol_is_not_a_variable_but_function = .{ .declaration = token_idx, .usage = identifier_index } });
                        return .ERROR;
                    },
                }
            }
        }

        for (0..symbol_table_stack.nesting_level + 1) |nl| {
            if (symbol_table_stack.undeclared[nl].contains(varname))
                return .ERROR; // nothing to do. this variable was already reported as undeclared.
        }

        // this variable apears the first time, is undeclared, and it must be reported!
        try symbol_table_stack.undeclared[symbol_table_stack.nesting_level].put(ti.gpa, varname, identifier_index);
        try ti.addError(.{ .undecl_var = identifier_index });

        return .ERROR;
    }

    fn inferIf(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .IF);

        const cond_idx = node.first_child;
        assert(cond_idx > 0);
        const cond_type = try ti.inferType(cond_idx, symbol_table_stack);
        try ti.expectType(cond_idx, cond_type, .BOOL);

        // get then block:
        const cond_node = ti.ast.get(cond_idx);
        const then_idx = cond_node.next_sibling;
        assert(then_idx > 0);
        _ = try ti.inferType(then_idx, symbol_table_stack);

        // get else block:
        const then_node = ti.ast.get(then_idx);
        const else_idx = then_node.next_sibling;
        if (else_idx > 0)
            _ = try ti.inferType(else_idx, symbol_table_stack);

        return .VOID;
    }

    fn inferWhile(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .WHILE);

        const cond_idx = node.first_child;
        assert(cond_idx > 0);
        const cond_type = try ti.inferType(cond_idx, symbol_table_stack);
        try ti.expectType(cond_idx, cond_type, .BOOL);

        // get body:
        const cond_node = ti.ast.get(cond_idx);
        const body_idx = cond_node.next_sibling;
        assert(body_idx > 0);
        _ = try ti.inferType(body_idx, symbol_table_stack);

        return .VOID;
    }

    fn inferBlock(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .BLOCK);
        var child_list = node.children(ti.ast);

        try symbol_table_stack.enterBlock();
        defer symbol_table_stack.exitBlock();

        while (child_list.nextIdx()) |child_idx|
            _ = try ti.inferType(child_idx, symbol_table_stack);

        return .VOID;
    }

    fn inferFunctionCall(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .FNCALL);
        const token = ti.tokens[node.token_index];
        assert(token.tag == .IDENTIFIER);
        const fname = token.str(ti.source);

        // TODO:
        // 1. find symbol
        const symbol = ti.global_symbol_table.get(fname) orelse {
            const undecl = &symbol_table_stack.undeclared[symbol_table_stack.nesting_level];
            if (!undecl.contains(fname)) {
                try undecl.put(ti.gpa, fname, node.token_index);
                try ti.addError(.{ .unknown_function = node_idx });
            }
            return .ERROR;
        };
        // 2. check symbol is a function
        switch (symbol.kind) {
            .function => |fn_template_idx| {
                assert(fn_template_idx != .NONE);
                // 3. infer types of arguments:
                var child_iterator = node.children(ti.ast);
                var arg_type_buf: [MAX_NUM_FUNCTION_PARAMS]DkType = undefined;
                var arg_count: usize = 0;
                while (child_iterator.nextIdx()) |arg_idx| {
                    const arg_type = try ti.inferType(arg_idx, symbol_table_stack);
                    arg_type_buf[arg_count] = arg_type;
                    arg_count += 1;
                }
                const arg_types = arg_type_buf[0..arg_count];

                const fn_template = ti.finfo.templates.get(fn_template_idx);
                return ti.instantiateFunction(node_idx, &fn_template, arg_types);
            },
            else => {
                try ti.addError(.{ .symbol_is_not_a_function = .{ .declaration = symbol.declaration, .call = node.token_index } });
                return .ERROR;
            },
        }

        // var maybe_fhead: ?FunctionHead = null;
        // for (builtin_functions) |fh| {
        //     if (std.mem.eql(u8, fh.name, fname))
        //         maybe_fhead = fh;
        // }

        // if (maybe_fhead == null) {
        //     try ti.addError(.{ .unknown_function = node_idx });
        //     return .ERROR;
        // }
        // const fhead = maybe_fhead.?;

        // var child_list = node.children(ti.ast);

        // var arg_num: usize = 0;
        // while (child_list.nextIdx()) |arg_idx| {
        //     if (arg_num < fhead.param_count) {
        //         assert(arg_num < MAX_NUM_FUNCTION_PARAMS);

        //         const arg_type = try ti.inferType(arg_idx);
        //         if (arg_type == .ERROR) {
        //             const param_type = fhead.param_types[arg_num];
        //             if (arg_type != param_type)
        //                 try ti.addError(.{ .wrong_type = .{ .ast_node = arg_idx, .expected = @tagName(param_type), .actual = arg_type } });
        //         }
        //     }
        //     arg_num += 1;
        // }

        // if (arg_num != fhead.param_count)
        //     try ti.addError(.{ .wrong_num_fun_args = .{ .ast_node = node_idx, .actual = @intCast(arg_num), .expected = fhead.param_count } });

        // return fhead.return_type;
    }

    fn inferType(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) TypeInfererException!DkType {
        const node = ti.ast.get(node_idx);
        const node_type = switch (node.tag) {
            .INVALID => unreachable,
            .FNDECL => unreachable,
            .FNPARAMS => unreachable,
            .RETURN => unreachable, // FIXME: handle or remove!
            .DECLARATION => try ti.inferDecl(node_idx, symbol_table_stack),
            .ASSIGNMENT => try ti.inferAssignment(node_idx, symbol_table_stack),
            .BINARY_OP => try ti.inferBinaryOp(node_idx, symbol_table_stack),
            .UNARY_OP => try ti.inferUnaryOp(node_idx, symbol_table_stack),
            .FNCALL => try ti.inferFunctionCall(node_idx, symbol_table_stack),
            .WHILE => try ti.inferWhile(node_idx, symbol_table_stack),
            .BLOCK => try ti.inferBlock(node_idx, symbol_table_stack),
            .ATOM => try ti.inferAtom(node_idx, symbol_table_stack),
            .IF => try ti.inferIf(node_idx, symbol_table_stack),
        };
        // ti.node_types[node_idx] = node_type;
        return node_type;
    }

    // fn inferReturnTypeOfFunctionBody(ti: *TypeInferer, fn_body_idx: AstNodeIndex, symbol_table_stack: *SymbolTableStack) TypeInfererException!DkType {
    //     var children = ti.ast.get(fn_body_idx).children(ti.ast);

    //     var return_type: DkType = .VOID;
    //     while (children.nextIdx()) |child_idx| {
    //         const child_node = ti.ast.get(child_idx);
    //         switch (child_node.tag) {
    //             .RETURN => {
    //                 const return_expr_idx = child_node.first_child;
    //                 if (return_expr_idx > 0) {
    //                     const return_expr_type = try ti.inferType(return_expr_idx, symbol_table_stack);
    //                     if (return_type == .VOID)
    //                         return_type = return_expr_type;
    //                     else if (return_expr_type != return_type)
    //                         try ti.addError(.{ .type_mismatch = .{ .ast_node = return_expr_idx, .lhs = return_type, .rhs = return_expr_type } });
    //                 }
    //         }
    //         const child_type = try ti.inferType(child_idx, symbol_table_stack);

    //         unreachable; // FIXME
    //     }

    //     return return_type;
    // }

    fn registerDeclaration(ti: *TypeInferer, identifier_index: TokenIndex, rhs_type: DkType, symbol_table_stack: *SymbolTableStack) !void {
        const token = ti.tokens[identifier_index];
        assert(token.tag == .IDENTIFIER);
        const varname = token.str(ti.source);

        var error_occured = false;
        // check nesting levels above current one:
        for (0..symbol_table_stack.nesting_level) |nl| {
            if (symbol_table_stack.tables[nl].get(varname)) |varinfo| {
                // already declared => error
                if (varinfo.kind == .result_variable) {
                    try ti.addError(.{ .redeclaration_of_result_var = identifier_index });
                } else {
                    try ti.addError(.{ .decl_shadows_outer = .{
                        .outer_decl = varinfo.declaration,
                        .error_decl = identifier_index,
                    } });
                }
                error_occured = true;
                break;
            }
        }

        if (!error_occured) {
            // check current nesting level:
            if (symbol_table_stack.tables[symbol_table_stack.nesting_level].get(varname)) |varinfo| {
                // already declared => error
                if (varinfo.kind == .result_variable) {
                    try ti.addError(.{ .redeclaration_of_result_var = identifier_index });
                } else {
                    try ti.addError(.{ .multi_decl_var = .{
                        .first_decl = varinfo.declaration,
                        .error_decl = identifier_index,
                    } });
                }
            } else {
                // add declaration
                try symbol_table_stack.tables[symbol_table_stack.nesting_level].put(
                    ti.gpa,
                    varname,
                    .{ .declaration = identifier_index, .kind = .{ .variable = rhs_type } },
                );
            }
        }
    }

    fn instantiateFunction(ti: *TypeInferer, fn_call_node_idx: AstNodeIndex, fn_template: *const FunctionTemplate, argument_types: []DkType) !DkType { // return type of instance
        if (fn_template.param_count != argument_types.len) {
            // this should never happen if the function templates are well-formed and the type checker is correctly implemented, but we check it just to be sure.
            try ti.addError(.{ .wrong_num_fun_args = .{ .ast_node = fn_call_node_idx, .actual = @intCast(argument_types.len), .expected = fn_template.param_count } });
            return error.WrongNumberOfFunctionArguments; // TODO: if we know the return type anyway, return it and continue type checking
        }

        const fn_name = fn_template.name(ti);

        if (ti.finfo.instances.getPtr(.{ .name = fn_name, .param_types = argument_types })) |existing_instance| {
            if (existing_instance.return_type == .UNKNOWN) {
                // this means that we are in a recursive call and the instance we found is the one we are currently instantiating
                // AND the return type was not specified manually.
                // in the future, we can try to infer the return type anyway. But for now, we just report an error.
                try ti.addError(.{ .return_type_missing_for_recursive_fn = fn_template.ast_idx });
                return .ERROR;
            } else {
                // this means that we either have already instantiated this function with these argument types,
                // or we are in a recursive call but the return type was manually specified.
                // in both cases, we can just return the return type of the existing instance.
                return existing_instance.return_type;
            }
            return existing_instance;
        }

        const fn_call_node = ti.ast.get(fn_call_node_idx);
        // const fn_arguments_node = ti.ast.get(fn_call_node.first_child);

        var argument_node_iterator = fn_call_node.children(ti.ast);
        for (fn_template.params(&ti.finfo.params), argument_types) |param, arg_type| {
            const fn_argument_node_idx = argument_node_iterator.nextIdx() orelse {
                std.debug.print("#### INTERNAL ERROR: argument node index out of bounds when instantiating function {s} param: {s} ##########\n\n", .{ fn_name, ti.ts.sourceStr(param.name_token_idx) });
                par.debugPrintAstBranch(fn_call_node_idx, ti.ast, ti.tokens, ti.source);
                unreachable;
            }; // for error reporting

            switch (param.type_) {
                .TYPEVAR => |typevar_id| {
                    _ = typevar_id; // we ignore this for now. Eventually we need to check if it was reused and if it was assigned to the same conrete type everywhere.
                    // param_type = arg_type;
                },
                .CONCRETE => |param_type| {
                    if (param_type != arg_type) {
                        try ti.addError(.{ .wrong_type = .{ .ast_node = fn_argument_node_idx, .expected = @tagName(param_type), .actual = arg_type } });
                        return .ERROR; // TODO: if we know the return type anyway, return it and continue type checking
                    }
                },
            }
        } // for param, arg_type, i, fn_argument_node_idx

        //     // add param with type to symbol table:
        //     const param_token = ti.tokens[param.name_token_idx];
        //     const param_name = param_token.str(ti.source);
        //     try symbol_table_stack[0].put(ti.gpa, param_name, .{ .declaration = param.name_token_idx, .kind = .variable(param_type) });
        // }

        try ti.fn_instantiation_ast_idx_stack.append(ti.gpa, fn_call_node_idx);
        defer { _ = ti.fn_instantiation_ast_idx_stack.pop(); }

        return ti.actuallyInstantiateFunction(fn_template, argument_types, fn_template.return_type);

        // Notes:
        // this method is recursive as functions can be instantiated within other functions.
        // we need to be careful to avoid infinite recursion when instantiating recursive functions.
        // we can do this by first adding an instance with the given template and argument types to the function instances before we start typechecking the function body.
        // then, if we encounter a call to the same function with the same argument types while typechecking the body,
        // we know that we are in a recursive call and we can just return the instance we already added without trying to instantiate it again.
        // In this case, the return type must be manually specified, for now. otherwise ==> error

        // The type checker must be modified, such that we have local versions of declarations and undeclared__ that are used when typechecking the function body.
        // maybe we can use a pool of symbol tables for this, where each function body gets its own symbol table from the pool when it is instantiated.
        // when we enter a function body, we push the symbol table for that body onto a stack, and when we exit the body, we pop the symbol table from the stack and return it to the pool.
        // this way we can easily handle nested function definitions without having to worry about manually merging and unmerging symbol tables.
        // for this to work, we need a separate symbol table for global symbols. (This may eventually become multi-layered if we allow several nested global scopes in the future.)

    }

    fn instantiateNonGenericFn(ti: *TypeInferer, fn_template: *const FunctionTemplate) !DkType {
        assert(fn_template.typevar_count == 0);

        const params = fn_template.params(&ti.finfo.params);

        var scratch: [MAX_NUM_FUNCTION_PARAMS]DkType = undefined;
        for (params, 0..) |param, i| {
            switch (param.type_) {
                .CONCRETE => |t| scratch[i] = t,
                .TYPEVAR => unreachable,
            }
        }
        const param_types = scratch[0..fn_template.param_count];
        assert(!ti.finfo.instances.contains(.{ .name = fn_template.name(ti), .param_types = param_types }));

        return try ti.actuallyInstantiateFunction(fn_template, param_types, fn_template.return_type);
    }

    fn actuallyInstantiateFunction(ti: *TypeInferer, fn_template: *const FunctionTemplate, param_types: []DkType, return_type: DkType) !DkType {
        assert(param_types.len == fn_template.param_count);

        const fn_name = fn_template.name(ti);
        const params = fn_template.params(&ti.finfo.params);

        var symbol_table_stack = try SymbolTableStack.init(&ti.symbol_table_pool);
        defer symbol_table_stack.deinit();

        // add a variable for the result type:
        try symbol_table_stack.tables[0].put(ti.gpa, "result", .{ .declaration = fn_template.ast_idx, .kind = .{ .result_variable = return_type } }); // FIXME: use the index of the return type decl here. Instead of the one of the function decl

        const instance_param_types = &ti.finfo.instances.param_types;

        // try instance_param_types.ensureCapacity(instance_param_types.items.len + fn_template.param_count);
        const first_param_type_idx = instance_param_types.items.len;
        for (params, param_types) |param, param_type| {
            _ = try instance_param_types.append(param_type);

            // add param with type to symbol table:
            const param_token = ti.tokens[param.name_token_idx];
            const param_name = param_token.str(ti.source);
            try symbol_table_stack.tables[0].put(ti.gpa, param_name, .{ .declaration = param.name_token_idx, .kind = .{ .fn_param = param_type } });
        }

        const stored_param_types = @TypeOf(instance_param_types.*).IndexRange.create(first_param_type_idx, fn_template.param_count);

        // add a an incomplete entry so we know, that this function is already in process of being instantiated.
        // This is used to detect recursion.
        const res = try ti.finfo.instances.put(.{ .name = fn_name, .param_types = stored_param_types }, FunctionInstance{
            .name_ = .{ .user_defined = fn_template.ast_idx },
            .param_count = fn_template.param_count,
            .first_param_type_idx = @enumFromInt(first_param_type_idx),
            .return_type = return_type, // may be .UNKNOWN, will be updated later in this case
        });
        const inst = res.value_ptr;
        try ti.fn_instantiation_stack.append(ti.gpa, res.index);
        defer { _ = ti.fn_instantiation_stack.pop(); }


        _ = try ti.inferType(fn_template.body_ast_idx, &symbol_table_stack);

        if (symbol_table_stack.tables[0].get("result")) |result_info| {
            const actual_type = result_info.kind.result_variable;
            switch (inst.return_type) {
                .UNKNOWN => inst.return_type = if (actual_type == .UNKNOWN) .VOID else actual_type, // if the return type is still unknown, this means that there were no return statements in the function body, so we can set the return type to void.
                else => {
                    if (inst.return_type != actual_type) {
                        unreachable; // this should have been caught while infering the type inside the function body.
                    }
                },
            }
        } else {
            unreachable;
        }

        return inst.return_type;
    }

    fn instantiateAllNonGenericFunctions(ti: *TypeInferer) !void {
        for (ti.finfo.templates.items[1..]) |*fn_template| {
            if (fn_template.typevar_count == 0)
                _ = try ti.instantiateNonGenericFn(fn_template);
        }
    }

    fn registerFunctionTemplate(ti: *TypeInferer, node_idx: AstNodeIndex, symbol_table: *SymbolTable) !void {
        const ft_node = ti.ast.get(node_idx);
        const ft_token = ti.tokens[ft_node.token_index];
        assert(ft_token.tag == .IDENTIFIER);
        const fn_name = ft_token.str(ti.source);

        // check for duplicate function name:
        if (symbol_table.get(fn_name)) |existing_fn_info| {
            // TODO: differentiate btw a duplicate function and a variable/constant with the same name

            // report duplicate function:
            try ti.addError(.{ .multi_decl_fn = .{
                .first_decl = existing_fn_info.declaration,
                .error_decl = ft_node.token_index,
            } });
            return;
        }

        // register all function parameters:
        const fn_params_node = ti.ast.get(ft_node.first_child);
        assert(fn_params_node.tag == .FNPARAMS);
        var child_list = fn_params_node.children(ti.ast);
        var param_count: u8 = 0;
        const first_param_idx = ti.finfo.params.items.len;
        while (child_list.nextIdx()) |param_idx| {
            const param_node = ti.ast.get(param_idx);

            // check is param shadows a global symbol:
            const param_name = ti.tokens[param_node.token_index].str(ti.source);
            if (symbol_table.get(param_name)) |existing_param| {
                try ti.addError(.{ .param_shadows_global = .{
                    .global_decl = existing_param.declaration,
                    .param_decl = param_node.token_index,
                } });
            }

            // variable seems ok. add it:
            _ = try ti.finfo.params.append(.{
                .name_token_idx = param_node.token_index,
                .type_ = .{ .TYPEVAR = param_count }, // TODO: for now, we treat all params as typevars. later we can add syntax for specifying fixed types for params.
            });
            param_count += 1;
        }

        // check if any parameters have the same name:
        for (0..param_count) |i| {
            for (i + 1..param_count) |j| {
                const param_i_token_idx = ti.finfo.params.items[first_param_idx + i].name_token_idx;
                const param_j_token_idx = ti.finfo.params.items[first_param_idx + j].name_token_idx;
                const param_i_name = ti.tokens[param_i_token_idx].str(ti.source);
                const param_j_name = ti.tokens[param_j_token_idx].str(ti.source);
                if (std.mem.eql(u8, param_i_name, param_j_name)) {
                    try ti.addError(.{ .function_parameters_have_same_name = .{
                        .first_param = param_i_token_idx,
                        .second_param = param_j_token_idx,
                    } });
                }
            }
        }

        const fn_body_ast_idx = fn_params_node.next_sibling;
        assert(fn_body_ast_idx > 0);

        const typevar_count = param_count; // TODO: for now, we treat all params as typevars.
        // create entry in function templates and add to symbol table.
        const ft_idx = try ti.finfo.templates.append(.{
            .ast_idx = node_idx,
            .body_ast_idx = fn_body_ast_idx,
            .param_count = param_count,
            .typevar_count = typevar_count,
            .first_param_idx = @enumFromInt(first_param_idx),
        });
        try symbol_table.put(ti.gpa, fn_name, .{ .declaration = ft_node.token_index, .kind = .{ .function = ft_idx } });
    }

    fn registerAllFunctionTemplates(ti: *TypeInferer, node_idx: AstNodeIndex) !void {
        // function declarations are only allowed at the root block level.

        const node = ti.ast.get(node_idx);
        if (node.tag != .BLOCK) return error.UnexpectedNode;

        var child_list = node.children(ti.ast);
        while (child_list.nextIdx()) |child_idx| {
            const child_node = ti.ast.get(child_idx);
            switch (child_node.tag) {
                .FNDECL => try ti.registerFunctionTemplate(child_idx, &ti.global_symbol_table),
                else => unreachable, // return error.UnexpectedNode, // TODO: add type declarations here later
            }
        }
    }

    pub fn checkAndReconstructTypes(ti: *TypeInferer, node_idx: AstNodeIndex) !void {
        try ti.registerAllFunctionTemplates(node_idx);
        if (ti.errors.items.len > 0)
            return error.ErrorsDuringFunctionTemplateRegistration;
        try ti.instantiateAllNonGenericFunctions();
        if (ti.errors.items.len > 0)
            return error.ErrorsDuringFunctionInstantiation;
    }

    pub fn printLineAndMarkAstNode(
        ti: *const TypeInferer,
        writer: *std.Io.Writer,
        ts: TokenStream,
        node_idx: AstNodeIndex,
        enforce_indent : ?usize,
    ) !SourceLoc {
        const node = ti.ast.get(node_idx);
        const main_token = ts.tokens[node.token_index];
        const token_line = ts.token_lines[node.token_index];
        const line_info = ts.line_infos[token_line];

        const line = ts.source[line_info.start..line_info.end];
        var num_whitespace : usize = 0;
        var additional_indent : usize = 0;
        if (enforce_indent) |indent| {
            additional_indent = indent;
            for (line) |c| {
                if (c == ' ') {
                    num_whitespace += 1;
                    
                } else {
                    break;
                }
            }   
        }

        // Print the source line.
        try writer.splatByteAll(' ', additional_indent);
        try writer.print("{s}\n", .{line[num_whitespace..]});

        const border_tokens = par.getFirstAndLastTokenOfAstBranch(node_idx, ti.ast);
        var start_char = ti.tokens[border_tokens.first].start;
        var end_char = ti.tokens[border_tokens.last].end();
        if( start_char < line_info.start ) {
            start_char = line_info.start;
            while(start_char < line_info.end and ti.source[start_char] == ' ')
                start_char += 1;
        }
        if( end_char > line_info.end ) {
            end_char = line_info.end;
            while(end_char > line_info.start and ti.source[end_char - 1] == ' ')
                end_char -= 1;
        }

        // Build underline buffer: space = no mark, '^' = main token, '~' = other tokens in branch.
        var underline: [256]u8 = .{' '} ** 256;
        var underline_len: usize = 0;

        {   
            const start_col = start_char-line_info.start;
            const end_col = @min(end_char-line_info.start, underline.len);
            @memset(underline[start_col..end_col], '"');
            underline_len = end_col;
        }
        // Main token overwrites with '^'.
        {   
            const start_col = main_token.start-line_info.start;
            const end_col = @min(main_token.end() - line_info.start, underline.len);
            @memset(underline[start_col..end_col], '^');
        }

        try writer.splatByteAll(' ', additional_indent);
        try writer.print("{s}\n", .{underline[num_whitespace..underline_len]});

        return .{ .line = token_line, .column = main_token.start - line_info.start };
    }


    pub fn printErrors(ti: *const TypeInferer, writer: *std.Io.Writer, ts: TokenStream) !void {
        const code_indent = 4;

        try writer.writeByte('\n');
        for (ti.errors.items) |error_info| {
            const inst_info = error_info.fn_instantiation_stack_top[0];
            const inst = ti.finfo.instances.getByIndex(inst_info.function);
            
            // try writer.splatByteAll(' ', indentation);
            try writer.print("in\nfn {s}(", .{inst.name(ti)});
            for(ti.finfo.paramTypesOfInstance(inst)) |param_type| {
                try writer.print("{s},", .{@tagName(param_type)});
            }
            try writer.writeAll("):\n");  


            const err = error_info.error_;
            // try writer.writeByte('\n');
            switch (err) {
                .undecl_var => |token_index| {
                    const pos = try ts.printLineAndMarkToken(writer, token_index);
                    try writer.print(
                        "line {}: Error: undeclared variable \"{s}\" (declare using the := operator).\n",
                        .{ pos.line, ts.sourceStr(token_index) },
                    );
                },
                .multi_decl_var => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.error_decl);
                    try writer.print(
                        "line {}: Error: symbol \"{s}\" has already been declared on line {}:\n",
                        .{ pos.line, ts.sourceStr(e.error_decl), ts.token_lines[e.first_decl] },
                    );
                    _ = try ts.printLineAndMarkToken(writer, e.first_decl);
                },
                .multi_decl_fn => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.error_decl);
                    try writer.print(
                        "line {}: Error: symbol \"{s}\" has already been declared on line {}:\n",
                        .{ pos.line, ts.sourceStr(e.error_decl), ts.token_lines[e.first_decl] },
                    );
                    _ = try ts.printLineAndMarkToken(writer, e.first_decl);
                },
                .decl_shadows_outer => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.error_decl);
                    try writer.print(
                        "line {}: Error: variable \"{s}\" shadows symbol with same name on line {}:\n",
                        .{ pos.line, ts.sourceStr(e.error_decl), ts.token_lines[e.outer_decl] },
                    );
                    _ = try ts.printLineAndMarkToken(writer, e.outer_decl);
                },
                .unknown_function => |node_idx| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, node_idx, code_indent);
                    const node = ti.ast.get(node_idx);
                    try writer.print(
                        "line {}: Error: unknown function \"{s}\".\n",
                        .{ pos.line, ts.sourceStr(node.token_index) },
                    );
                },
                .wrong_type => |e| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, e.ast_node, code_indent);
                    try writer.print(
                        "line {}: Error: wrong type: {s}. Expected: {s}.\n",
                        .{ pos.line, @tagName(e.actual), e.expected },
                    );
                },
                .type_mismatch => |e| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, e.ast_node, code_indent);
                    try writer.print(
                        "line {}: Error: type mismatch: LHS: {s}. RHS: {s}.\n",
                        .{ pos.line, @tagName(e.lhs), @tagName(e.rhs) },
                    );
                },
                .fn_params_are_immutable => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.usage);
                    try writer.print(
                        "line {}: Error: function parameters are immutable, but parameter \"{s}\" is assigned a value.\n",
                        .{ pos.line, ts.sourceStr(e.declaration) },
                    );
                },
                .wrong_num_fun_args => |e| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, e.ast_node, code_indent);
                    try writer.print(
                        "line {}: Error: wrong number of function arguments: {}. Expected: {}.\n",
                        .{ pos.line, e.actual, e.expected },
                    );
                },
                .symbol_is_not_a_function => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.call);
                    try writer.print(
                        "line {}: Error: symbol \"{s}\" is not a function (declared on line {}).\n",
                        .{ pos.line, ts.sourceStr(e.call), ts.token_lines[e.declaration] },
                    );
                },
                .symbol_is_not_a_variable_but_function => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.usage);
                    try writer.print(
                        "line {}: Error: symbol \"{s}\" is not a variable but a function (declared on line {}).\n",
                        .{ pos.line, ts.sourceStr(e.usage), ts.token_lines[e.declaration] },
                    );
                },
                .redeclaration_of_result_var => |token_index| {
                    const pos = try ts.printLineAndMarkToken(writer, token_index);
                    try writer.print(
                        "line {}: Error: redeclaration of reserved result variable \"result\".\n",
                        .{pos.line},
                    );
                },
                .param_shadows_global => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.param_decl);
                    try writer.print(
                        "line {}: Error: parameter \"{s}\" shadows global variable declared on line {}:\n",
                        .{ pos.line, ts.sourceStr(e.param_decl), ts.token_lines[e.global_decl] },
                    );
                },
                .function_parameters_have_same_name => |e| {
                    // TODO: mark both tokens instead of only the second one
                    const pos = try ts.printLineAndMarkToken(writer, e.second_param);
                    try writer.print(
                        "line {}: Error: parameter \"{s}\" has same name as previous parameter on line {}:\n",
                        .{ pos.line, ts.sourceStr(e.second_param), ts.token_lines[e.first_param] },
                    );
                },
                .return_type_missing_for_recursive_fn => |node_idx| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, node_idx, code_indent);
                    const node = ti.ast.get(node_idx);
                    try writer.print(
                        "line {}: Error: cannot infer return type for recursive function \"{s}\". Return type must be explicitly declared.\n",
                        .{ pos.line, ts.sourceStr(node.token_index) },
                    );
                },
                .result_type_mismatch => |e| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, e.ast_node, code_indent);
                    try writer.print(
                        "line {}: Error: result type mismatch: Result type should be: {s}. But result is assigned {s}.\n",
                        .{ pos.line, @tagName(e.lhs), @tagName(e.rhs) },
                    );
                    
                },
            }
            try ti.printInstantiationSack(writer, error_info, 4, 1);
            try writer.writeByte('\n');
        }
        
        try writer.flush();
    }

    fn printInstantiationSack(ti : *const TypeInferer, writer: *std.Io.Writer, error_info : ErrorInfo, indentation: usize, first_entry_to_print:usize) !void {
        try writer.writeByte('\n');
        try writer.splatByteAll(' ', indentation);
        try writer.writeAll("function instantiation stack:\n\n");
        for (error_info.fn_instantiation_stack_top[first_entry_to_print..error_info.fn_instantiation_stack_len]) |inst_info| {
            const inst = ti.finfo.instances.getByIndex(inst_info.function);
            
            try writer.splatByteAll(' ', indentation);
            try writer.print("fn {s}(", .{inst.name(ti)});
            for(ti.finfo.paramTypesOfInstance(inst)) |param_type| {
                try writer.print("{s},", .{@tagName(param_type)});
            }

            try writer.writeAll(")\n");            
            
            if(inst_info.fn_call_ast_idx != 0) {
                const line_num = ti.ts.token_lines[ti.ast.get(inst_info.fn_call_ast_idx).token_index];
                try writer.splatByteAll(' ', indentation);
                try writer.print("    line {}:\n", .{line_num});
                const pos = try ti.printLineAndMarkAstNode(writer, ti.ts, inst_info.fn_call_ast_idx, indentation+4);
                _ = pos;
            }
            else {
                try writer.writeByte('\n');
            }
        }
    }

};

// -----------------------------------------------------------------------


pub const FunctionTemplate = struct {
    ast_idx: AstNodeIndex,
    body_ast_idx: AstNodeIndex,
    first_param_idx: FunctionParams.Index = .NONE,
    param_count: u8 = 0,
    typevar_count: u8 = 0,
    return_type: DkType = .UNKNOWN,

    pub fn name(self: *const FunctionTemplate, ti: *const TypeInferer) []const u8 {
        assert(self.ast_idx != 0);
        const ast_node = ti.ast.get(self.ast_idx);
        switch(ast_node.tag) {
            .FNDECL => {},
            else => {
                std.debug.print("INTERNAL COMPILER ERROR: function template: AST node is not a FNDECL: {any}\n", .{ ast_node });
                unreachable;
            },
        }
        const name_token = ti.tokens[ast_node.token_index];
        switch(name_token.tag) {
            .IDENTIFIER => return name_token.str(ti.source),
            else => {
                std.debug.print("INTERNAL COMPILER ERROR: function template name token is not an identifier. token: {any} \"{s}\"\n", .{ name_token, name_token.str(ti.source) });
                unreachable;
            },
        }
        
    }

    pub fn params(self: *const FunctionTemplate, params_array: *const FunctionParams) []const FunctionParam {
        return params_array.slice(self.first_param_idx, self.param_count);
    }

    pub const null_element = FunctionTemplate{
        .ast_idx = 0,
        .body_ast_idx = 0,
        .first_param_idx = .NONE,
        .param_count = 0,
        .typevar_count = 0,
        .return_type = .UNKNOWN,
    };
};

const TypeVarId = u8; // FIXME

pub const FunctionParam = struct {
    name_token_idx: TokenIndex = 0,
    type_: union(enum) {
        CONCRETE: DkType,
        TYPEVAR: TypeVarId,
    } = .{ .CONCRETE = .UNKNOWN },
};

pub const FunctionTemplates = util.ArrayList(FunctionTemplate);
pub const FunctionParams = util.ArrayList(FunctionParam);
pub const Types = util.ArrayList(DkType);

pub const FunctionInstance = struct {
    name_: union(enum) {
        builtin: []const u8,
        user_defined: AstNodeIndex, // index of function head in ast
    },
    return_type: DkType = .UNKNOWN,
    param_count: u8 = 0,
    first_param_type_idx: Types.Index = .NONE, // index to type of first param. the next param types are at subsequent indices. (i.e. param_types + 1, param_types + 2, etc.)
    // body_type_offset: u32 = 0, // given the AST-branch for this function, the index of the type for each AST node can be found by indexOf(node) + body_type_offset.

    pub fn name(self: *const FunctionInstance, ti: *const TypeInferer) []const u8 {
        switch (self.name_) {
            .builtin => return self.name_.builtin,
            .user_defined => {
                const ast_node = ti.ast.get(self.name_.user_defined);
                const name_token = ti.tokens[ast_node.token_index];
                assert(name_token.tag == .IDENTIFIER);
                return name_token.str(ti.source);
            },
        }
    }
};

pub const FunctionInstanceKey = struct {
    name: []const u8,
    param_types: Types.IndexRange, // types of parameters. the length of this array is equal to the param_count field of FunctionInstance
};
pub const FunctionInstancePseudoKey = struct {
    name: []const u8,
    param_types: []DkType,
};

pub const FunctionInstanceHashContext = struct {
    types: *const Types, // needed to resolve the type enums in the keys to actual types for hashing and equality checks

    fn combineHash(h: u64, t: DkType) u64 {
        return h ^ (@as(u64, @intFromEnum(t)) +% 0x9e3779b9 +% (h << 6) +% (h >> 2)); // from boost's hash_combine function
    }

    fn hashFunctionInstanceKey(ctx: FunctionInstanceHashContext, key: FunctionInstanceKey) u64 {
        var h = std.hash_map.hashString(key.name);
        var it = key.param_types.iterator();
        while (it.next()) |i| {
            const t = ctx.types.get(i);
            h = combineHash(h, t);
        }

        return h;
    }

    fn hashFunctionInstancePseudoKey(ctx: FunctionInstanceHashContext, key: FunctionInstancePseudoKey) u64 {
        _ = ctx;
        var h = std.hash_map.hashString(key.name);
        for (key.param_types) |t| {
            h = combineHash(h, t);
        }

        return h;
    }

    pub fn hash(ctx: FunctionInstanceHashContext, key: anytype) u32 {
        return @intCast(0xffffffff & switch (@TypeOf(key)) {
            FunctionInstanceKey => ctx.hashFunctionInstanceKey(key),
            FunctionInstancePseudoKey => ctx.hashFunctionInstancePseudoKey(key),
            else => unreachable,
        });
    }

    pub fn eql(ctx: FunctionInstanceHashContext, a: anytype, b: FunctionInstanceKey, index : usize) bool {
        _ = index;
        switch (@TypeOf(a)) {
            FunctionInstanceKey => return ctx.eqlKey(a, b),
            FunctionInstancePseudoKey => return ctx.eqlPseudoKey(a, b),
            else => unreachable,
        }
    }

    fn eqlPseudoKey(ctx: FunctionInstanceHashContext, a: FunctionInstancePseudoKey, b: FunctionInstanceKey) bool {
        if (a.param_types.len != b.param_types.len())
            return false;

        if (!std.mem.eql(u8, a.name, b.name))
            return false;

        for (a.param_types, ctx.types.sliceFromRange(b.param_types)) |a_type, b_type| {
            if (b_type != a_type)
                return false;
        }

        return true;
    }

    fn eqlKey(ctx: FunctionInstanceHashContext, a: FunctionInstanceKey, b: FunctionInstanceKey) bool {
        if (a.param_types.len() != b.param_types.len())
            return false;

        if (!std.mem.eql(u8, a.name, b.name))
            return false;

        for (ctx.types.sliceFromRange(a.param_types), ctx.types.sliceFromRange(b.param_types)) |a_type, b_type| {
            if (b_type != a_type)
                return false;
        }

        return true;
    }
};

pub const FunctionInstances = struct {
    // hash_map: std.hash_map.HashMapUnmanaged(FunctionInstanceKey, FunctionInstance, FunctionInstanceHashContext, 75),
    hash_map: HashMap,
    gpa: std.mem.Allocator,
    param_types: Types,

    pub const HashMap = std.ArrayHashMapUnmanaged(FunctionInstanceKey, FunctionInstance, FunctionInstanceHashContext,true);
    pub const Index = usize;

    const CTX = FunctionInstanceHashContext;

    pub fn init( gpa: std.mem.Allocator) !FunctionInstances {
        return FunctionInstances{
            .hash_map = .empty,
            .gpa = gpa,
            .param_types = try .initWithNullElement(gpa, 128, DkType.UNKNOWN),
        };
    }

    pub fn deinit(self: *FunctionInstances) void {
        self.hash_map.deinit(self.gpa);
        self.param_types.deinit();
    }

    pub fn getPtr(self: *FunctionInstances, key: FunctionInstancePseudoKey) ?*const FunctionInstance {
        return self.hash_map.getPtrAdapted(key, CTX{.types=&self.param_types});
    }

    pub fn get(self: *FunctionInstances, key: FunctionInstancePseudoKey) ?FunctionInstance {
        return self.hash_map.getAdapted(key, CTX{.types=&self.param_types});
    }

    pub fn getByIndex(self: *const FunctionInstances, index: Index) FunctionInstance {
        return self.hash_map.entries.get(index).value;
    }

    pub fn contains(self: *FunctionInstances, key: FunctionInstancePseudoKey) bool {
        return self.hash_map.containsAdapted(key, CTX{.types=&self.param_types});
    }

    pub fn getIndex(self: *const FunctionInstances, key: FunctionInstancePseudoKey) ?Index {
        return self.hash_map.getIndexAdapted(key, CTX{.types=&self.param_types});
    }

    pub fn put(self: *FunctionInstances, key: FunctionInstanceKey, value: FunctionInstance) !HashMap.GetOrPutResult {
        const res = try self.hash_map.getOrPutContextAdapted(self.gpa, key, CTX{.types=&self.param_types}, CTX{.types=&self.param_types});
        assert(!res.found_existing);
        res.key_ptr.* = key;
        res.value_ptr.* = value;
        return res;
    }
};
const FunctionInstanceIndex = FunctionInstances.Index;

pub const FunctionInfos = struct {
    templates: FunctionTemplates,
    params: FunctionParams,
    instances: FunctionInstances,
    // instance_param_types: Types,

    pub fn init(gpa: std.mem.Allocator) !FunctionInfos {
        return FunctionInfos{
            .templates = try .initWithNullElement(gpa, 32, FunctionTemplate.null_element),
            .params = try .init(gpa, 128),
            .instances = try .init(gpa),
        };
    }

    pub fn deinit(self: *FunctionInfos) void {
        self.templates.deinit();
        self.params.deinit();
        self.instances.deinit();
    }

    pub fn paramTypesOfInstance(self: *const FunctionInfos, instance: FunctionInstance) []const DkType {
        assert(instance.first_param_type_idx != .NONE);
        return self.instances.param_types.slice(instance.first_param_type_idx, instance.param_count);
    }
};


// -----------------------------------------------------------------------

// pub fn printAstBranchWithTypes(p: *const Parser, writer: *std.Io.Writer, node_types: []DkType, ast_index: AstNodeIndex, indentation: usize) !void {
//     try writer.splatByteAll(' ', 4 * indentation);

//     if (ast_index == 0) {
//         try writer.writeAll("INVALID\n");
//         return;
//     }

//     const node_type = node_types[ast_index];

//     const node = p.ast.get(ast_index).*;
//     const token = p.tokens[node.token_index];

//     try writer.print("{s} ({s}[{s}]) :{s} #{} (next_sibling={})\n", .{
//         @tagName(node.tag),
//         @tagName(token.tag),
//         if (token.tag == .EOL) "" else token.str(p.source),
//         @tagName(node_type),
//         ast_index,
//         p.ast.get(ast_index).next_sibling,
//     });

//     if (node.hasChild()) {
//         var child_list = node.children(&p.ast);
//         while (child_list.nextIdx()) |child_idx|
//             try printAstBranchWithTypes(p, writer, node_types, child_idx, indentation + 1);
//     }
// }

test "TypeInferer" {
    // const source =
    //     \\ x := -5.0 # declaring and assigning a variable
    //     \\ x = 2.0  # assigning an existing variable
    //     \\ d := 1
    //     \\ y := -x + -3 * - (-7 + -2) **-x + d
    //     \\ z := x ** y; p := 7.1
    //     \\ result := z + p**2
    // ;

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

    var stdout_buff: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stderr().writer(&stdout_buff);
    const stdout = &stdout_writer.interface;

    try stdout.writeAll(source);
    try stdout.writeByte('\n');
    try stdout.flush();

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    std.debug.print("tokens: {}\n", .{ts.tokens.len});

    var parser = try Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();
    _ = try parser.parse();
    // try parser.printAllNodesFlat(stdout);
    // try stdout.flush();

    const snapshot = try parser.ast.takeSnapshot();
    defer parser.ast.freeSnapshot(snapshot);

    std.debug.print("ast nodes: {}\n", .{parser.ast.nodes.items.len});

    if (parser.errors.items.len > 0) {
        try stdout.writeAll("\nError in the parsing stage:\n");
        try parser.printErrors(stdout, ts);
        return;
    }

    // try parser.printAstBranch(stdout, parser.root_node, 1);
    // try stdout.flush();

    // validate shit:
    var ti = try TypeInferer.init(gpa, ts, &parser);
    defer ti.deinit();

    try std.testing.expect(parser.ast.equals(snapshot));
    ti.checkAndReconstructTypes(parser.root_node) catch |err| {
        try stdout.print("\nError in the type inference stage: {}\n", .{err});
        
    };
    try ti.printErrors(stdout, ts);
    
    try stdout.flush();

    // try printAstBranchWithTypes(&parser, stdout, ti.node_types, parser.root_node, 1);
    // try stdout.flush();

    // try stdout.writeAll("Variables: ");
    // for(ti.declared_variables) |v| {

    // }
}
