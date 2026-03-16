const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");

const assert = std.debug.assert;

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;
const SourceLoc = tok.SourceLoc;

const AST = par.AST;
const AstNode = par.AstNode;
const Parser = par.Parser;
const AstNodeIndex = par.AstNodeIndex;

const MAX_NUM_FUNCTION_PARAMS = 14;

const FunctionHead = struct {
    name: []const u8,
    return_type: DkType = .UNKNOWN,
    param_count: u8 = 0,
    param_types: [MAX_NUM_FUNCTION_PARAMS]DkType = [1]DkType{.UNKNOWN} ** MAX_NUM_FUNCTION_PARAMS,
};

const builtin_functions = [_]FunctionHead{
    .{ .name = "print", .return_type = .VOID, .param_count = 1, .param_types = [1]DkType{.ANY} ++ [1]DkType{.VOID} ** (MAX_NUM_FUNCTION_PARAMS - 1) },
};

const DkType = enum(u8) {
    UNKNOWN = 0,
    VOID,
    BOOL,
    INT,
    FLOAT,
    ANY,
    ERROR,
};

const VariableInfo = struct {
    declaration_token: TokenIndex,
    infered_type: DkType,
};

pub const TypeInferer = struct {
    source: []const u8,
    tokens: []const Token,
    ast: *const AST,
    node_types: []DkType, // types for each node in the ast
    root_index: AstNodeIndex,
    gpa: std.mem.Allocator,

    // Each of the following stacks has 1 entry per block nesting level.
    // I.e. at index 0 are the declared(/undeclared__) identefiers of the root block.
    // An immediate child block of the root block will be at index 1, its children at 2, etc.
    declarations: [MAX_NESTING_LEVEL]std.hash_map.StringHashMapUnmanaged(VariableInfo), // used as a stack
    undeclared__: [MAX_NESTING_LEVEL]std.hash_map.StringHashMapUnmanaged(TokenIndex), // used as a stack
    nesting_level: usize = 0,
    // declared_variables: std.hash_map.StringHashMapUnmanaged(TokenIndex),
    // undeclared_variables: std.hash_map.StringHashMapUnmanaged(TokenIndex),
    errors: std.ArrayList(Error) = .empty,

    const MAX_NESTING_LEVEL = 16;

    pub const Error = union(enum) {
        undecl_var: TokenIndex,
        multi_decl_var: struct {
            first_decl: TokenIndex,
            error_decl: TokenIndex,
        },
        decl_shadows_outer: struct {
            outer_decl: TokenIndex,
            error_decl: TokenIndex,
        },
        unknown_function: AstNodeIndex,

        wrong_num_fun_args: struct {
            ast_node: AstNodeIndex,
            expected: u8,
            actual: u8,
        },

        // typing errors
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
    };

    pub const TypeInfererException = error{OutOfMemory};

    pub fn init(
        gpa: std.mem.Allocator,
        parser: *const Parser,
    ) !TypeInferer {
        var ti = TypeInferer{
            .source = parser.source,
            .tokens = parser.tokens,
            .ast = &parser.ast,
            .node_types = try gpa.alloc(DkType, parser.ast.nodeCount()),
            .root_index = parser.root_node,
            .gpa = gpa,
            .declarations = undefined,
            .undeclared__ = undefined,
        };

        for (0..MAX_NESTING_LEVEL) |i| {
            ti.declarations[i] = .{};
            ti.undeclared__[i] = .{};
        }

        return ti;
    }

    pub fn deinit(ti: *TypeInferer) void {
        for (0..MAX_NESTING_LEVEL) |i| {
            ti.declarations[i].deinit(ti.gpa);
            ti.undeclared__[i].deinit(ti.gpa);
        }
        ti.errors.deinit(ti.gpa);
        ti.gpa.free(ti.node_types);
    }

    fn enterBlock(ti: *TypeInferer) void {
        ti.nesting_level += 1;
        if (ti.nesting_level >= MAX_NESTING_LEVEL)
            return error.NestingLevelExceedsMaximum;
        // ti.declarations[ti.nesting_level].clearRetainingCapacity();
        // ti.undeclared__[ti.nesting_level].clearRetainingCapacity();
    }

    fn exitBlock(ti: *TypeInferer) void {
        std.debug.assert(ti.nesting_level > 0);
        ti.declarations[ti.nesting_level].clearRetainingCapacity();
        ti.undeclared__[ti.nesting_level].clearRetainingCapacity();

        // TODO
        ti.nesting_level -= 1;
    }

    fn typecheckSameType(ti: *TypeInferer, op_index: AstNodeIndex, lhs: AstNodeIndex, rhs: AstNodeIndex) !DkType {
        const lhs_type = try ti.evalTypeOf(lhs);
        const rhs_type = try ti.evalTypeOf(rhs);

        if (lhs_type == .ERROR or rhs_type == .ERROR)
            return .ERROR; // this error is a folowup error so we do not report it.

        if (lhs_type != rhs_type or lhs_type == .UNKNOWN) {
            try ti.errors.append(ti.gpa, .{ .type_mismatch = .{ .ast_node = op_index, .lhs = lhs_type, .rhs = rhs_type } });
            return .ERROR;
        }

        return .BOOL;
    }

    fn typeArithmeticBinaryOp(ti: *TypeInferer, op_idx: AstNodeIndex, lhs: AstNodeIndex, rhs: AstNodeIndex) !DkType {
        const lhs_type = try ti.evalTypeOf(lhs);
        const rhs_type = try ti.evalTypeOf(rhs);

        if (lhs_type == .ERROR or rhs_type == .ERROR)
            return .ERROR; // this error is a folowup error so we do not report it.

        var err_occured = false;
        if (lhs_type != .INT and lhs_type != .FLOAT) {
            try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = lhs, .actual = lhs_type, .expected = "LHS must be of number type" } });
            err_occured = true;
        }
        if (rhs_type != .INT and rhs_type != .FLOAT) {
            try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = rhs, .actual = rhs_type, .expected = "RHS must be of number type" } });
            err_occured = true;
        }

        if (err_occured)
            return .ERROR;

        if (lhs_type != rhs_type or lhs_type == .UNKNOWN) {
            try ti.errors.append(ti.gpa, .{ .type_mismatch = .{ .ast_node = op_idx, .lhs = lhs_type, .rhs = rhs_type } });
            return .ERROR;
        }

        return lhs_type;
    }

    fn typecheckBooleanBinaryOp(ti: *TypeInferer, lhs: AstNodeIndex, rhs: AstNodeIndex) !DkType {
        const lhs_type = try ti.evalTypeOf(lhs);
        const rhs_type = try ti.evalTypeOf(rhs);

        if (lhs_type == .ERROR or rhs_type == .ERROR)
            return .ERROR; // this error is a folowup error so we do not report it.

        var err_occured = false;
        if (lhs_type != .BOOL) {
            try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = lhs, .actual = lhs_type, .expected = "LHS must be a boolean" } });
            err_occured = true;
        }
        if (rhs_type != .BOOL) {
            try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = rhs, .actual = rhs_type, .expected = "RHS must be a number" } });
            err_occured = true;
        }

        if (err_occured)
            return .ERROR;

        return .BOOL;
    }

    fn typecheckBinaryOp(ti: *TypeInferer, op_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(op_idx);
        const token = ti.tokens[node.token_index];
        const lhs = node.first_child;
        const rhs = ti.ast.get(lhs).next_sibling;
        assert(lhs != 0);
        assert(rhs != 0);
        switch (token.tag) {
            .EQ, .NOT_EQ, .LT, .LE, .GT, .GE => return ti.typecheckSameType(op_idx, lhs, rhs),
            .PLUS, .MINUS, .DIV, .TIMES, .POW => return ti.typeArithmeticBinaryOp(op_idx, lhs, rhs),
            .AND, .OR, .XOR => return ti.typecheckBooleanBinaryOp(lhs, rhs),
            else => {
                std.debug.print("#### INTERNAL ERROR: node: {any} token: {any} ##########\n\n", .{node, token});
                unreachable;
            },
        }
    }

    fn typecheckUnaryOp(ti: *TypeInferer, op_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(op_idx);
        const token = ti.tokens[node.token_index];
        const child_idx = node.first_child;
        const child_type = try ti.evalTypeOf(child_idx);
        switch (token.tag) {
            .NOT => switch (child_type) {
                .ERROR => return .ERROR,
                .BOOL => return .BOOL,
                else => {
                    try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = child_idx, .actual = child_type, .expected = "Child expression must be a boolean" } });
                    return .ERROR;
                },
            },
            .MINUS => switch (child_type) {
                .ERROR => return .ERROR,
                .INT, .FLOAT => return child_type,
                else => {
                    try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = child_idx, .actual = child_type, .expected = "Child expression must be a number" } });
                    return .ERROR;
                },
            },
            else => unreachable,
        }
    }

    fn expectType(ti: *TypeInferer, node_idx: AstNodeIndex, actual: DkType, comptime expected: DkType) !void {
        if (actual != expected)
            try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = node_idx, .actual = actual, .expected = "must be of type " ++ @tagName(expected) } });

        return;
    }

    fn typecheckDecl(ti: *TypeInferer, node_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .DECLARATION);

        // get LHS:
        assert(node.first_child != 0);
        const var_node = ti.ast.get(node.first_child);
        const var_token_index = var_node.token_index;
        const var_token = ti.tokens[var_token_index];

        assert(var_token.tag == .IDENTIFIER);

        // typecheck RHS:
        const rhs_type = try ti.evalTypeOf(var_node.next_sibling);

        try ti.registerDeclaration(var_token_index, rhs_type);

        return rhs_type;
    }

    fn typecheckAssignment(ti: *TypeInferer, op_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(op_idx);
        const op_token = ti.tokens[node.token_index];

        const lhs_idx = node.first_child;
        assert(lhs_idx > 0);

        // get LHS:
        const var_node = ti.ast.get(lhs_idx);
        const var_token_index = var_node.token_index;
        const var_token = ti.tokens[var_token_index];
        assert(var_token.tag == .IDENTIFIER);

        const rhs_idx = var_node.next_sibling;
        assert(rhs_idx > 0);

        return switch (op_token.tag) {
            // zig fmt: off
            .PLUSASSIGN,
            .MINUSASSIGN,
            .MULTASSIGN,
            .DIVASSIGN    => try ti.typeArithmeticBinaryOp(op_idx, lhs_idx, rhs_idx),
            .ASSIGN       => try ti.typecheckSameType(op_idx ,lhs_idx, rhs_idx),
            else          => unreachable,
            // zig fmt: on
        };
    }

    fn typecheckAtom(ti: *TypeInferer, node_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .ATOM);
        const token = ti.tokens[node.token_index];
        return switch (token.tag) {
            // zig fmt: off
            .TRUE, .FALSE => .BOOL,
            .INT_LIT      => .INT,
            .FLOAT_LIT    => .FLOAT,
            .IDENTIFIER   => ti.getTypeOfVariable(node.token_index),
            // zig fmt: on
            else => unreachable,
        };
    }

    fn getTypeOfVariable(ti: *TypeInferer, identifier_index: TokenIndex) !DkType {
        const token = ti.tokens[identifier_index];
        std.debug.assert(token.tag == .IDENTIFIER);
        const varname = token.str(ti.source);

        for (0..ti.nesting_level + 1) |nl| {
            if (ti.declarations[nl].get(varname)) |var_info| {
                return var_info.infered_type; // was declared. we are happy!
            }
        }

        for (0..ti.nesting_level + 1) |nl| {
            if (ti.undeclared__[nl].contains(varname))
                return .ERROR; // nothing to do this variable was already reported as undeclared.
        }

        // this variable apears the first time, is undeclared, and it must be reported!
        try ti.undeclared__[ti.nesting_level].put(ti.gpa, varname, identifier_index);
        try ti.errors.append(ti.gpa, .{ .undecl_var = identifier_index });

        return .ERROR;
    }

    fn typecheckIf(ti: *TypeInferer, node_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .WHILE);

        const cond_idx = node.first_child;
        assert(cond_idx > 0);
        const cond_type = try ti.evalTypeOf(cond_idx);
        try ti.expectType(cond_idx, cond_type, .BOOL);

        // get then block:
        const cond_node = ti.ast.get(cond_idx);
        const then_idx = cond_node.next_sibling;
        assert(then_idx > 0);
        _ = try ti.evalTypeOf(then_idx);

        // get else block:
        const then_node = ti.ast.get(then_idx);
        const else_idx = then_node.next_sibling;
        if (else_idx > 0)
            _ = try ti.evalTypeOf(else_idx);

        return .VOID;
    }

    fn typecheckWhile(ti: *TypeInferer, node_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .WHILE);

        const cond_idx = node.first_child;
        assert(cond_idx > 0);
        const cond_type = try ti.evalTypeOf(cond_idx);
        try ti.expectType(cond_idx, cond_type, .BOOL);

        // get body:
        const cond_node = ti.ast.get(cond_idx);
        const body_idx = cond_node.next_sibling;
        assert(body_idx > 0);
        _ = try ti.evalTypeOf(body_idx);

        return .VOID;
    }

    fn typecheckBlock(ti: *TypeInferer, node_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .BLOCK);
        var child_list = node.children(ti.ast);

        while (child_list.nextIdx()) |child_idx|
            _ = try ti.evalTypeOf(child_idx);

        return .VOID;
    }

    fn typecheckFunctionCall(ti: *TypeInferer, node_idx: AstNodeIndex) !DkType {
        const node = ti.ast.get(node_idx);
        assert(node.tag == .FNCALL);
        const token = ti.tokens[node.token_index];
        assert(token.tag == .IDENTIFIER);
        const fname = token.str(ti.source);

        var maybe_fhead: ?FunctionHead = null;
        for (builtin_functions) |fh| {
            if (std.mem.eql(u8, fh.name, fname))
                maybe_fhead = fh;
        }

        if (maybe_fhead == null) {
            try ti.errors.append(ti.gpa, .{ .unknown_function = node_idx });
            return .ERROR;
        }
        const fhead = maybe_fhead.?;

        var child_list = node.children(ti.ast);

        var arg_num : usize = 0;
        while (child_list.nextIdx()) |arg_idx| {
            if (arg_num < fhead.param_count) {
                assert(arg_num < MAX_NUM_FUNCTION_PARAMS);

                const arg_type = try ti.evalTypeOf(arg_idx);
                if (arg_type == .ERROR) {
                    const param_type = fhead.param_types[arg_num];
                    if (arg_type != param_type)
                        try ti.errors.append(ti.gpa, .{ .wrong_type = .{ .ast_node = arg_idx, .expected = @tagName(param_type), .actual = arg_type } });
                }
            }
            arg_num += 1;
        }

        if (arg_num != fhead.param_count)
            try ti.errors.append(ti.gpa, .{ .wrong_num_fun_args = .{ .ast_node = node_idx, .actual = @intCast(arg_num), .expected = fhead.param_count } });

        return fhead.return_type;
    }

    fn evalTypeOf(ti: *TypeInferer, node_idx: AstNodeIndex) TypeInfererException!DkType {
        const node = ti.ast.get(node_idx);
        const node_type = switch (node.tag) {
            .INVALID => unreachable,
            .DECLARATION => try ti.typecheckDecl(node_idx),
            .ASSIGNMENT => try ti.typecheckAssignment(node_idx),
            .BINARY_OP => try ti.typecheckBinaryOp(node_idx),
            .UNARY_OP => try ti.typecheckUnaryOp(node_idx),
            .FNCALL => try ti.typecheckFunctionCall(node_idx),
            .WHILE => try ti.typecheckWhile(node_idx),
            .BLOCK => try ti.typecheckBlock(node_idx),
            .ATOM => try ti.typecheckAtom(node_idx),
            .IF => try ti.typecheckIf(node_idx),
        };
        ti.node_types[node_idx] = node_type;
        return node_type;
    }

    fn registerDeclaration(ti: *TypeInferer, identifier_index: TokenIndex, rhs_type: DkType) !void {
        const token = ti.tokens[identifier_index];
        assert(token.tag == .IDENTIFIER);
        const varname = token.str(ti.source);

        var error_occured = false;
        // check nesting levels above current one:
        for (0..ti.nesting_level) |nl| {
            if (ti.declarations[nl].get(varname)) |varinfo| {
                // already declared => error
                try ti.errors.append(ti.gpa, .{ .decl_shadows_outer = .{
                    .outer_decl = varinfo.declaration_token,
                    .error_decl = identifier_index,
                } });
                error_occured = true;
                break;
            }
        }

        if (!error_occured) {
            // check current nesting level:
            if (ti.declarations[ti.nesting_level].get(varname)) |varinfo| {
                // already declared => error
                try ti.errors.append(ti.gpa, .{ .multi_decl_var = .{
                    .first_decl = varinfo.declaration_token,
                    .error_decl = identifier_index,
                } });
            } else {
                // add declaration
                try ti.declarations[ti.nesting_level].put(
                    ti.gpa,
                    varname,
                    .{ .declaration_token = identifier_index, .infered_type = rhs_type },
                );
            }
        }
    }

    // fn validateVariableUsage(ti: *TypeInferer, identifier_index: TokenIndex) !void {
    //     const token = ti.tokens[identifier_index];
    //     std.debug.assert(token.tag == .IDENTIFIER);
    //     const varname = token.str(ti.source);

    //     for (0..ti.nesting_level + 1) |nl| {
    //         if (ti.declarations[nl].contains(varname))
    //             return; // was declared. we are happy!
    //     }

    //     for (0..ti.nesting_level + 1) |nl| {
    //         if (ti.undeclared__[nl].contains(varname))
    //             return; // nothing to do this variable was already reported as undeclared.
    //     }

    //     // this variable apears the first time and is must be reported!
    //     try ti.undeclared__[ti.nesting_level].put(ti.gpa, varname, identifier_index);
    //     try ti.errors.append(ti.gpa, .{ .undecl_var = identifier_index });
    // }

    // fn validateExpression(ti: *TypeInferer, node_idx: AstNodeIndex) TypeInfererException!void {
    //     assert(node_idx > 0);
    //     const node = ti.ast.get(node_idx).*;
    //     switch (node.tag) {
    //         .ATOM => {
    //             const token = ti.tokens[node.token_index];
    //             if (token.tag == .IDENTIFIER)
    //                 try ti.validateVariableUsage(node.token_index);
    //         },
    //         .FNCALL => try ti.validateFnCall(node),
    //         .UNARY_OP => try ti.validateExpression(node.first_child),
    //         .BINARY_OP => {
    //             // std.debug.print("binray op: {}.first_child=={}.next_sibling=={}\n", .{ node_idx, node.first_child, ti.ast.get(node.first_child).next_sibling });
    //             try ti.validateExpression(node.first_child);
    //             try ti.validateExpression(ti.ast.get(node.first_child).next_sibling);
    //         },
    //         else => unreachable,
    //     }
    // }

    // fn validateFnCall(ti: *TypeInferer, node: AstNode) !void {
    //     const token = ti.tokens[node.token_index];

    //     for (builtin_functions) |funcname| {
    //         if (!std.mem.eql(u8, funcname, token.str(ti.source))) {
    //             try ti.errors.append(ti.gpa, .{ .unknown_function = node.token_index });
    //         }
    //         var argument_list = node.children(ti.ast);
    //         while (argument_list.nextIdx()) |child_idx|
    //             try ti.validateExpression(child_idx);
    //     }
    // }

    // fn validateAssignment(ti: *TypeInferer, node: AstNode) !void {
    //     std.debug.assert(node.tag == .ASSIGNMENT);
    //     const var_node = ti.ast.get(node.first_child);
    //     const lhs_var_token_index = var_node.token_index;
    //     const lhs_variable = ti.tokens[lhs_var_token_index];
    //     // const lhs_varname = lhs_variable.str(ti.source);
    //     std.debug.assert(lhs_variable.tag == .IDENTIFIER);

    //     try ti.validateVariableUsage(lhs_var_token_index);

    //     try ti.validateExpression(var_node.next_sibling);
    // }

    // fn validateDeclaration(ti: *TypeInferer, node: AstNode) !void {
    //     std.debug.assert(node.tag == .DECLARATION);
    //     const var_node = ti.ast.get(node.first_child);
    //     const lhs_var_token_index = var_node.token_index;
    //     const lhs_variable = ti.tokens[lhs_var_token_index];
    //     // const lhs_varname = lhs_variable.str(ti.source);
    //     std.debug.assert(lhs_variable.tag == .IDENTIFIER);

    //     try ti.validateExpression(var_node.next_sibling);

    //     try ti.registerDeclaration(lhs_var_token_index);
    // }

    // fn validateWhile(ti: *TypeInferer, node: AstNode) !void {
    //     try ti.validateExpression(node.first_child);
    //     try ti.validateBlock(ti.ast.get(node.first_child).next_sibling);
    // }

    // fn validateIf(ti: *TypeInferer, node: AstNode) !void {
    //     const children = node.conditionThenElse(ti.ast);
    //     try ti.validateExpression(children.cond_idx);
    //     try ti.validateBlock(children.then_idx);
    //     if (children.else_idx != 0)
    //         try ti.validateBlock(children.else_idx);
    // }

    // fn validateStatement(ti: *TypeInferer, node_idx: AstNodeIndex) !void {
    //     assert(node_idx > 0);

    //     const node = ti.ast.get(node_idx).*;
    //     switch (node.tag) {
    //         // zig fmt: off
    //         .ASSIGNMENT  => try ti.validateAssignment(node),
    //         .DECLARATION => try ti.validateDeclaration(node),
    //         .WHILE       => try ti.validateWhile(node),
    //         .IF          => try ti.validateIf(node),
    //         .FNCALL      => try ti.validateFnCall(node),
    //         // zig fmt: on
    //         else => unreachable,
    //     }
    // }

    // fn validateBlock(ti: *TypeInferer, node_idx: AstNodeIndex) TypeInfererException!void {
    //     assert(node_idx > 0);

    //     const node = ti.ast.get(node_idx);
    //     std.debug.assert(node.tag == .BLOCK);
    //     var children = node.children(ti.ast);
    //     while (children.nextIdx()) |child_idx| {
    //         try ti.validateStatement(child_idx);
    //     }
    // }

    // pub fn validate(ti: *TypeInferer) !void {
    //     try ti.validateBlock(ti.root_index);

    //     // return error.NotImplemented;
    //     // for (ti.statement_indices) |ci| {
    //     //     try ti.validateStatement(ci);
    //     // }

    //     // if (!ti.declared_variables.contains("result"))
    //     //     try ti.errors.append(ti.gpa, .{ .no_result = {} });
    // }

    pub fn checkAndReconstructTypes(ti: *TypeInferer, node_idx: AstNodeIndex) !void {
        _ = try ti.evalTypeOf(node_idx);
    }

    pub fn printLineAndMarkAstNode(
        ti: *const TypeInferer,
        writer: *std.Io.Writer,
        ts : TokenStream,
        node_idx: AstNodeIndex,
    ) !SourceLoc {
        const node = ti.ast.get(node_idx);

        // FIXME!!!
        return ts.printLineAndMarkToken(writer, node.token_index);
    }

    pub fn printErrors(ti: *const TypeInferer, writer: *std.Io.Writer, ts: TokenStream) !void {
        for (ti.errors.items) |err| {
            switch (err) {
                .undecl_var => |token_index| {
                    const pos = try ts.printLineAndMarkToken(writer, token_index);
                    try writer.print(
                        "line {}: Error: undeclared variable \"{s}\" (declare using the := operator).\n\n",
                        .{ pos.line, ts.sourceStr(token_index) },
                    );
                },
                .multi_decl_var => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.error_decl);
                    try writer.print(
                        "line {}: Error: variable \"{s}\" has already been declared on line {}:\n",
                        .{ pos.line, ts.sourceStr(e.error_decl), ts.token_lines[e.first_decl] },
                    );
                    _ = try ts.printLineAndMarkToken(writer, e.first_decl);
                    try writer.writeByte('\n');
                },
                .decl_shadows_outer => |e| {
                    const pos = try ts.printLineAndMarkToken(writer, e.error_decl);
                    try writer.print(
                        "line {}: Error: variable \"{s}\" shadows variable with same name on line {}:\n",
                        .{ pos.line, ts.sourceStr(e.error_decl), ts.token_lines[e.outer_decl] },
                    );
                    _ = try ts.printLineAndMarkToken(writer, e.outer_decl);
                    try writer.writeByte('\n');
                },
                .unknown_function => |node_idx| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, node_idx);
                    const node = ti.ast.get(node_idx);
                    try writer.print(
                        "line {}: Error: unknown function \"{s}\".\n",
                        .{ pos.line, ts.sourceStr(node.token_index) },
                    );
                    try writer.writeByte('\n');
                },
                .wrong_type => |e| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, e.ast_node);
                    try writer.print(
                        "line {}: Error: wrong type: {s}. Expected: {s}.\n",
                        .{ pos.line, @tagName(e.actual), e.expected },
                    );
                    try writer.writeByte('\n');
                },
                .type_mismatch => |e| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, e.ast_node);
                    try writer.print(
                        "line {}: Error: type missmatch: LHS: {s}. RHS: {s}.\n",
                        .{ pos.line, @tagName(e.lhs), @tagName(e.rhs) },
                    );
                    try writer.writeByte('\n');
                },
                .wrong_num_fun_args => |e| {
                    const pos = try ti.printLineAndMarkAstNode(writer, ts, e.ast_node);
                    try writer.print(
                        "line {}: Error: wrong number of function arguments: {}. Expected: {}.\n",
                        .{ pos.line, e.actual, e.expected },
                    );
                    try writer.writeByte('\n');
                },
            }
        }
        try writer.flush();
    }
};

test "TypeInferer" {
    const source =
        \\ x := -5.0 # declaring and assigning a variable
        \\ x = 2.0  # assigning an existing variable
        \\ d := 1
        \\ y := -x + -3 * - (-7 + -2) **-x + d
        \\ z := x ** y; p := 7.1
        \\ result := z + p**2
    ;

    var stdout_buff: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stderr().writer(&stdout_buff);
    const stdout = &stdout_writer.interface;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    std.debug.print("tokens: {}\n", .{ts.tokens.len});

    var parser = try Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();
    _ = try parser.parse();
    try parser.printAllNodesFlat(stdout);
    try stdout.flush();

    const snapshot = try parser.ast.takeSnapshot();
    defer parser.ast.freeSnapshot(snapshot);

    std.debug.print("ast nodes: {}\n", .{parser.ast.nodes.items.len});

    if (parser.errors.items.len > 0)
        try parser.printErrors(stdout, ts);

    try parser.printAstBranch(stdout, parser.root_node, 1);
    try stdout.flush();

    // validate shit:
    var ti = try TypeInferer.init(gpa, &parser);
    defer ti.deinit();

    try std.testing.expect(parser.ast.equals(snapshot));
    try ti.checkAndReconstructTypes( parser.root_node);
    try ti.printErrors(stdout, ts);
    try stdout.flush();
}
