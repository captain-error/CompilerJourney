const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const ft_ast = @import("ft_ast.zig");
const type_inference = @import("type_inference.zig");

const assert = std.debug.assert;

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

const AST = par.AST;
const AstNode = par.AstNode;
const AstNodeIndex = par.AstNodeIndex;
const Parser = par.Parser;

const DkType = type_inference.DkType;
const TypeInferer = type_inference.TypeInferer;
const FunctionInfos = type_inference.FunctionInfos;
const FunctionTemplate = type_inference.FunctionTemplate;
const FunctionInstance = type_inference.FunctionInstance;
const FunctionInstances = type_inference.FunctionInstances;
const FunctionInstancePseudoKey = type_inference.FunctionInstancePseudoKey;
const FunctionParams = type_inference.FunctionParams;

const FtAst = ft_ast.FtAst;
const Scope = ft_ast.Scope;
const VarDecl = ft_ast.VarDecl;
const FnDecl = ft_ast.FnDecl;
const Expression = ft_ast.Expression;
const Statement = ft_ast.Statement;
const ScopeIndex = ft_ast.ScopeIndex;
const StamentIndex = ft_ast.StamentIndex;
const ExpressionIndex = ft_ast.ExpressionIndex;
const VarDeclIndex = ft_ast.VarDeclIndex;
const FnDeclIndex = ft_ast.FnDeclIndex;

const MAX_NUM_FUNCTION_PARAMS = 32;

const ScopeKind = @FieldType(Scope, "kind");
const BinaryOp = ft_ast.BinaryOp;
const UnaryOp = ft_ast.UnaryOp;
const AssignmentKind = ft_ast.AssignmentKind;

pub fn lower(
    gpa: std.mem.Allocator,
    parser: *const Parser,
    ti: *const TypeInferer,
) !FtAst {
    var ft = try FtAst.init(gpa);
    errdefer ft.deinit();

    var builder = FtAstBuilder{
        .ast = &parser.ast,
        .tokens = parser.tokens,
        .source = parser.source,
        .finfo = &ti.finfo,
        .name_resolution = ti.name_resolution,
        .ft = &ft,
        .decl_map = .empty,
        .instance_map = try gpa.alloc(FnDeclIndex, ti.finfo.instances.hash_map.entries.len),
        .gpa = gpa,
    };
    defer builder.decl_map.deinit(gpa);
    defer gpa.free(builder.instance_map);

    try builder.createAllFnDecls();
    try builder.lowerAllBodies();

    return ft;
}

const FtAstBuilder = struct {
    // Inputs (read-only):
    ast: *const AST,
    tokens: []const Token,
    source: []const u8,
    finfo: *const FunctionInfos,
    name_resolution: []const TokenIndex,

    // Output:
    ft: *FtAst,

    // Working state:
    decl_map: std.AutoHashMapUnmanaged(TokenIndex, VarDeclIndex),
    instance_map: []FnDeclIndex, // indexed by finfo.instances entry index

    gpa: std.mem.Allocator,

    const LowerError = error{OutOfMemory};

    // -----------------------------------------------------------------------
    // Pass 1: create all FnDecl headers
    // -----------------------------------------------------------------------

    fn createAllFnDecls(b: *FtAstBuilder) LowerError!void {
        const instances = &b.finfo.instances;
        const entries = instances.hash_map.entries;

        for (0..entries.len) |inst_idx| {
            const inst = entries.get(inst_idx).value;

            // Skip builtins — they have no AST body
            switch (inst.name_) {
                .builtin => continue,
                .user_defined => {},
            }

            const fn_ast_idx = inst.name_.user_defined;
            const template = b.findTemplate(fn_ast_idx);

            // Arena-allocate function name
            const fn_name_token = b.tokens[b.ast.get(fn_ast_idx).token_index];
            const fn_name = try b.ft.arena.allocator().dupe(u8, fn_name_token.str(b.source));

            // Create params (contiguous → IndexRange)
            const params_start: VarDeclIndex = @intCast(b.ft.var_decls.items.len);
            const template_params = template.params(&b.finfo.params);
            const param_types = b.finfo.paramTypesOfInstance(inst);

            for (template_params, param_types) |param, ptype| {
                const param_name_token = b.tokens[param.name_token_idx];
                const param_name = try b.ft.arena.allocator().dupe(u8, param_name_token.str(b.source));
                try b.ft.var_decls.append(b.gpa, .{
                    .name = param_name,
                    .type_ = ptype,
                    .kind = .FN_PARAM,
                    .parent_scope = 0, // set in pass 2
                });
            }
            const params_end: VarDeclIndex = @intCast(b.ft.var_decls.items.len);

            // Create FnDecl
            const fn_decl_idx: FnDeclIndex = @intCast(b.ft.fn_decls.items.len);
            try b.ft.fn_decls.append(b.gpa, .{
                .name = fn_name,
                .return_type = inst.return_type,
                .params = .{ .start = params_start, .end = params_end },
                .body_scope = 0, // filled in pass 2
            });

            b.instance_map[inst_idx] = fn_decl_idx;
        }
    }

    fn findTemplate(b: *const FtAstBuilder, fn_ast_idx: AstNodeIndex) *const FunctionTemplate {
        for (b.finfo.templates.items[1..]) |*tmpl|
            if (tmpl.ast_idx == fn_ast_idx) return tmpl;

        unreachable;
    }

    // -----------------------------------------------------------------------
    // Pass 2: lower all function bodies
    // -----------------------------------------------------------------------

    fn lowerAllBodies(b: *FtAstBuilder) LowerError!void {
        const instances = &b.finfo.instances;
        const entries = instances.hash_map.entries;

        for (0..entries.len) |inst_idx| {
            const inst = entries.get(inst_idx).value;

            switch (inst.name_) {
                .builtin => continue,
                .user_defined => {},
            }

            const fn_ast_idx = inst.name_.user_defined;
            const template = b.findTemplate(fn_ast_idx);
            const fn_decl_idx = b.instance_map[inst_idx];

            // Clear per-instantiation state
            b.decl_map.clearRetainingCapacity();

            // Create body scope
            const scope_idx: ScopeIndex = @intCast(b.ft.scopes.items.len);
            try b.ft.scopes.append(b.gpa, .{ .kind = .FN, .parent_scope = 0 });

            // Create result VarDecl
            const result_decl_idx: VarDeclIndex = @intCast(b.ft.var_decls.items.len);
            try b.ft.var_decls.append(b.gpa, .{
                .name = try b.ft.arena.allocator().dupe(u8, "result"),
                .type_ = inst.return_type,
                .kind = .RESULT,
                .parent_scope = scope_idx,
            });
            // Key for result in name_resolution is fn_template.ast_idx (stored as TokenIndex)
            try b.decl_map.put(b.gpa, fn_ast_idx, result_decl_idx);

            // Link result into scope's first_decl
            b.ft.scopes.items[scope_idx].first_decl = result_decl_idx;

            // Populate decl_map with params and set parent_scope
            const fn_decl = &b.ft.fn_decls.items[fn_decl_idx];
            const template_params = template.params(&b.finfo.params);
            const params_start = fn_decl.params.start;
            for (template_params, 0..) |param, i| {
                const var_decl_idx: VarDeclIndex = params_start + @as(VarDeclIndex, @intCast(i));
                b.ft.var_decls.items[var_decl_idx].parent_scope = scope_idx;
                try b.decl_map.put(b.gpa, param.name_token_idx, var_decl_idx);
            }

            // Lower the body block
            try b.lowerBlockInto(template.body_ast_idx, scope_idx);

            b.ft.fn_decls.items[fn_decl_idx].body_scope = scope_idx;
        }
    }

    // -----------------------------------------------------------------------
    // Block lowering
    // -----------------------------------------------------------------------

    fn lowerBlock(b: *FtAstBuilder, block_ast_idx: AstNodeIndex, kind: ScopeKind, parent_scope: ScopeIndex) LowerError!ScopeIndex {
        const scope_idx: ScopeIndex = @intCast(b.ft.scopes.items.len);
        try b.ft.scopes.append(b.gpa, .{ .kind = kind, .parent_scope = parent_scope });
        try b.lowerBlockInto(block_ast_idx, scope_idx);
        return scope_idx;
    }

    fn lowerBlockInto(b: *FtAstBuilder, block_ast_idx: AstNodeIndex, scope_idx: ScopeIndex) LowerError!void {
        const block_node = b.ast.get(block_ast_idx);
        assert(block_node.tag == .BLOCK);

        var child_iter = block_node.children(b.ast);
        var prev_stmt_idx: StamentIndex = 0;

        while (child_iter.nextIdx()) |child_ast_idx| {
            const stmt_idx = try b.lowerStatement(child_ast_idx, scope_idx);

            if (prev_stmt_idx == 0)
                b.ft.scopes.items[scope_idx].first_statement = stmt_idx
            else
                b.ft.statements.items[prev_stmt_idx].next_sibling = stmt_idx;

            prev_stmt_idx = stmt_idx;
        }
    }

    // -----------------------------------------------------------------------
    // Statement lowering
    // -----------------------------------------------------------------------

    fn lowerStatement(b: *FtAstBuilder, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) LowerError!StamentIndex {
        const node = b.ast.get(ast_idx);
        return switch (node.tag) {
            .DECLARATION => b.lowerDeclaration(ast_idx, parent_scope),
            .ASSIGNMENT => b.lowerAssignment(ast_idx),
            .IF => b.lowerIf(ast_idx, parent_scope),
            .WHILE => b.lowerWhile(ast_idx, parent_scope),
            .FNCALL => b.lowerFnCallStatement(ast_idx),
            else => unreachable,
        };
    }

    fn lowerDeclaration(b: *FtAstBuilder, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) LowerError!StamentIndex {
        const node = b.ast.get(ast_idx);
        const lhs_ast = b.ast.get(node.first_child);
        const rhs_ast_idx = lhs_ast.next_sibling;

        const rhs_expr = try b.lowerExpression(rhs_ast_idx);
        const rhs_type = b.ft.expressions.items[rhs_expr].type_;

        const var_name_token = b.tokens[lhs_ast.token_index];
        const var_name = try b.ft.arena.allocator().dupe(u8, var_name_token.str(b.source));

        const var_decl_idx: VarDeclIndex = @intCast(b.ft.var_decls.items.len);
        try b.ft.var_decls.append(b.gpa, .{
            .name = var_name,
            .type_ = rhs_type,
            .kind = .VAR,
            .parent_scope = parent_scope,
        });

        // Prepend to scope's decl list
        b.ft.var_decls.items[var_decl_idx].next_sibling = b.ft.scopes.items[parent_scope].first_decl;
        b.ft.scopes.items[parent_scope].first_decl = var_decl_idx;

        // Register in decl_map
        try b.decl_map.put(b.gpa, lhs_ast.token_index, var_decl_idx);

        const stmt_idx: StamentIndex = @intCast(b.ft.statements.items.len);
        try b.ft.statements.append(b.gpa, .{
            .kind = .{ .VAR_DECL = .{ .var_decl_idx = var_decl_idx, .rhs = rhs_expr } },
        });
        return stmt_idx;
    }

    fn lowerAssignment(b: *FtAstBuilder, ast_idx: AstNodeIndex) LowerError!StamentIndex {
        const node = b.ast.get(ast_idx);
        const op_token = b.tokens[node.token_index];
        const lhs_ast_idx = node.first_child;
        const lhs_ast = b.ast.get(lhs_ast_idx);
        const rhs_ast_idx = lhs_ast.next_sibling;

        const rhs_expr = try b.lowerExpression(rhs_ast_idx);
        const rhs_type = b.ft.expressions.items[rhs_expr].type_;

        const decl_token_idx = b.name_resolution[lhs_ast_idx];
        assert(decl_token_idx != 0);
        const var_decl_idx = b.decl_map.get(decl_token_idx).?;
        const var_decl = b.ft.var_decls.items[var_decl_idx];

        const stmt_idx: StamentIndex = @intCast(b.ft.statements.items.len);

        if (var_decl.kind == .RESULT) {
            try b.ft.statements.append(b.gpa, .{
                .kind = .{ .RESULT_ASSIGN = .{ .type_ = rhs_type, .rhs = rhs_expr } },
            });
        } else {
            const assign_kind: AssignmentKind = switch (op_token.tag) {
                // zig fmt: off
                .ASSIGN      => .ASSIGN,
                .PLUSASSIGN   => .PLUS,
                .MINUSASSIGN => .MINUS,
                .MULTASSIGN  => .MULT,
                .DIVASSIGN   => .DIV,
                // zig fmt: on
                else => unreachable,
            };
            try b.ft.statements.append(b.gpa, .{
                .kind = .{ .ASSIGNMENT = .{
                    .var_decl_idx = var_decl_idx,
                    .type_ = rhs_type,
                    .kind = assign_kind,
                    .rhs = rhs_expr,
                } },
            });
        }
        return stmt_idx;
    }

    fn lowerIf(b: *FtAstBuilder, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) LowerError!StamentIndex {
        const node = b.ast.get(ast_idx);
        const cte = node.conditionThenElse(b.ast);

        const cond_expr = try b.lowerExpression(cte.cond_idx);
        const then_scope = try b.lowerBlock(cte.then_idx, .IF, parent_scope);
        const else_scope: ScopeIndex = if (cte.else_idx != 0)
            try b.lowerBlock(cte.else_idx, .ELSE, parent_scope)
        else
            0;

        const stmt_idx: StamentIndex = @intCast(b.ft.statements.items.len);
        try b.ft.statements.append(b.gpa, .{
            .kind = .{ .IF_STMT = .{
                .condition = cond_expr,
                .then_scope = then_scope,
                .else_scope = else_scope,
            } },
        });
        return stmt_idx;
    }

    fn lowerWhile(b: *FtAstBuilder, ast_idx: AstNodeIndex, parent_scope: ScopeIndex) LowerError!StamentIndex {
        const node = b.ast.get(ast_idx);
        const cond_idx = node.first_child;
        const body_ast_idx = b.ast.get(cond_idx).next_sibling;

        const cond_expr = try b.lowerExpression(cond_idx);
        const body_scope = try b.lowerBlock(body_ast_idx, .WHILE, parent_scope);

        const stmt_idx: StamentIndex = @intCast(b.ft.statements.items.len);
        try b.ft.statements.append(b.gpa, .{
            .kind = .{ .WHILE_LOOP = .{
                .condition = cond_expr,
                .body_scope = body_scope,
            } },
        });
        return stmt_idx;
    }

    fn lowerFnCallStatement(b: *FtAstBuilder, ast_idx: AstNodeIndex) LowerError!StamentIndex {
        const expr_idx = try b.lowerExpression(ast_idx);

        const stmt_idx: StamentIndex = @intCast(b.ft.statements.items.len);
        try b.ft.statements.append(b.gpa, .{
            .kind = .{ .FN_CALL = expr_idx },
        });
        return stmt_idx;
    }

    // -----------------------------------------------------------------------
    // Expression lowering
    // -----------------------------------------------------------------------

    fn lowerExpression(b: *FtAstBuilder, ast_idx: AstNodeIndex) LowerError!ExpressionIndex {
        const node = b.ast.get(ast_idx);
        return switch (node.tag) {
            .ATOM => b.lowerAtom(ast_idx),
            .BINARY_OP => b.lowerBinaryOp(ast_idx),
            .UNARY_OP => b.lowerUnaryOp(ast_idx),
            .FNCALL => b.lowerFnCall(ast_idx),
            else => unreachable,
        };
    }

    fn lowerAtom(b: *FtAstBuilder, ast_idx: AstNodeIndex) LowerError!ExpressionIndex {
        const node = b.ast.get(ast_idx);
        const token = b.tokens[node.token_index];

        const expr_idx: ExpressionIndex = @intCast(b.ft.expressions.items.len);

        switch (token.tag) {
            .INT_LIT => {
                const val = std.fmt.parseInt(i64, token.str(b.source), 10) catch unreachable;
                try b.ft.expressions.append(b.gpa, .{
                    .type_ = .INT,
                    .kind = .{ .LITERAL = .{ .int = val } },
                });
            },
            .FLOAT_LIT => {
                const val = std.fmt.parseFloat(f64, token.str(b.source)) catch unreachable;
                try b.ft.expressions.append(b.gpa, .{
                    .type_ = .FLOAT,
                    .kind = .{ .LITERAL = .{ .float = val } },
                });
            },
            .TRUE => {
                try b.ft.expressions.append(b.gpa, .{
                    .type_ = .BOOL,
                    .kind = .{ .LITERAL = .{ .boolean = true } },
                });
            },
            .FALSE => {
                try b.ft.expressions.append(b.gpa, .{
                    .type_ = .BOOL,
                    .kind = .{ .LITERAL = .{ .boolean = false } },
                });
            },
            .IDENTIFIER => {
                const decl_token_idx = b.name_resolution[ast_idx];
                assert(decl_token_idx != 0);
                const var_decl_idx = b.decl_map.get(decl_token_idx).?;
                const var_type = b.ft.var_decls.items[var_decl_idx].type_;
                try b.ft.expressions.append(b.gpa, .{
                    .type_ = var_type,
                    .kind = .{ .VAR_REF = var_decl_idx },
                });
            },
            else => unreachable,
        }
        return expr_idx;
    }

    fn lowerBinaryOp(b: *FtAstBuilder, ast_idx: AstNodeIndex) LowerError!ExpressionIndex {
        const node = b.ast.get(ast_idx);
        const token = b.tokens[node.token_index];

        const lhs_ast_idx = node.first_child;
        const rhs_ast_idx = b.ast.get(lhs_ast_idx).next_sibling;

        const lhs_expr = try b.lowerExpression(lhs_ast_idx);
        const rhs_expr = try b.lowerExpression(rhs_ast_idx);

        const lhs_type = b.ft.expressions.items[lhs_expr].type_;

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

        const result_type: DkType = switch (token.tag) {
            .EQ, .NOT_EQ, .LT, .LE, .GT, .GE, .AND, .OR, .XOR => .BOOL,
            .PLUS, .MINUS, .TIMES, .DIV, .POW => lhs_type,
            else => unreachable,
        };

        const expr_idx: ExpressionIndex = @intCast(b.ft.expressions.items.len);
        try b.ft.expressions.append(b.gpa, .{
            .type_ = result_type,
            .kind = .{ .BINARY_OP = .{ .op = op, .lhs = lhs_expr, .rhs = rhs_expr } },
        });
        return expr_idx;
    }

    fn lowerUnaryOp(b: *FtAstBuilder, ast_idx: AstNodeIndex) LowerError!ExpressionIndex {
        const node = b.ast.get(ast_idx);
        const token = b.tokens[node.token_index];

        const operand_expr = try b.lowerExpression(node.first_child);
        const operand_type = b.ft.expressions.items[operand_expr].type_;

        const op: UnaryOp = switch (token.tag) {
            .MINUS => .NEGATE,
            .NOT => .BOOL_NOT,
            else => unreachable,
        };

        const result_type: DkType = switch (token.tag) {
            .MINUS => operand_type,
            .NOT => .BOOL,
            else => unreachable,
        };

        const expr_idx: ExpressionIndex = @intCast(b.ft.expressions.items.len);
        try b.ft.expressions.append(b.gpa, .{
            .type_ = result_type,
            .kind = .{ .UNARY_OP = .{ .op = op, .operand = operand_expr } },
        });
        return expr_idx;
    }

    fn lowerFnCall(b: *FtAstBuilder, ast_idx: AstNodeIndex) LowerError!ExpressionIndex {
        const node = b.ast.get(ast_idx);
        assert(node.tag == .FNCALL);
        const fn_name_token = b.tokens[node.token_index];
        const fn_name = fn_name_token.str(b.source);

        // Lower all argument expressions, collecting types
        var arg_types_buf: [MAX_NUM_FUNCTION_PARAMS]DkType = undefined;
        var arg_count: usize = 0;
        var first_arg_expr: ExpressionIndex = 0;
        var prev_arg_expr: ExpressionIndex = 0;

        var child_iter = node.children(b.ast);
        while (child_iter.nextIdx()) |arg_ast_idx| {
            const arg_expr = try b.lowerExpression(arg_ast_idx);
            arg_types_buf[arg_count] = b.ft.expressions.items[arg_expr].type_;
            arg_count += 1;

            if (first_arg_expr == 0)
                first_arg_expr = arg_expr
            else
                b.ft.expressions.items[prev_arg_expr].next_sibling = arg_expr;

            prev_arg_expr = arg_expr;
        }

        const arg_types = arg_types_buf[0..arg_count];

        // Look up the function instance
        const inst_idx = b.finfo.instances.getIndex(.{
            .name = fn_name,
            .param_types = arg_types,
        }).?;

        const fn_decl_idx = b.instance_map[inst_idx];
        const return_type = b.ft.fn_decls.items[fn_decl_idx].return_type;

        const expr_idx: ExpressionIndex = @intCast(b.ft.expressions.items.len);
        try b.ft.expressions.append(b.gpa, .{
            .type_ = return_type,
            .kind = .{ .FN_CALL = .{ .function = fn_decl_idx, .args_start = first_arg_expr } },
        });
        return expr_idx;
    }
};

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

test "lower zinseszins program" {
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

    var parser = try Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();
    _ = try parser.parse();
    try std.testing.expect(!parser.hasErrors());

    var ti = try TypeInferer.init(gpa, ts, &parser);
    defer ti.deinit();

    ti.checkAndReconstructTypes(parser.root_node) catch |err| {
        std.debug.print("Type inference error: {}\n", .{err});
        return error.TypeInferenceError;
    };
    try std.testing.expect(!ti.hasErrors());

    var ft = try lower(gpa, &parser, &ti);
    defer ft.deinit();

    // Should have 3 function declarations (do_zins, zinseszins, main)
    // fn_decls[0] is the invalid sentinel
    try std.testing.expectEqual(@as(usize, 4), ft.fn_decls.items.len);

    // Each function should have a valid body scope
    for (ft.fn_decls.items[1..]) |fn_decl|
        try std.testing.expect(fn_decl.body_scope != 0);
}
