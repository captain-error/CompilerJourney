const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");

const exdat = @import("extra_data.zig");

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

const AstNode = par.AstNode;
const Parser = par.Parser;
const AstNodeIndex = par.AstNodeIndex;

const ExtraData = exdat.ExtraData;
const ExtraIndex = exdat.ExtraIndex;

const builtin_functions = [_][:0]const u8{
    "print",
};

pub const SemanticValidator = struct {
    source: []const u8,
    tokens: []const Token,
    ast_nodes: []const AstNode,
    extra: *const ExtraData,
    root_index: AstNodeIndex,
    gpa: std.mem.Allocator,

    // Each of the following stacks has 1 entry per block nesting level.
    // I.e. at index 0 are the declared(/undeclared__) identefiers of the root block.
    // An immediate child block of the root block will be at index 1, its children at 2, etc.
    declarations: [MAX_NESTING_LEVEL]std.hash_map.StringHashMapUnmanaged(TokenIndex), // used as a stack
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
        unknown_function: TokenIndex,
    };

    pub const SemanticValidatorException = error{OutOfMemory};

    pub fn init(
        gpa: std.mem.Allocator,
        parser: *const Parser,
    ) SemanticValidator {
        var v = SemanticValidator{
            .source = parser.source,
            .tokens = parser.tokens,
            .ast_nodes = parser.ast_nodes.items,
            .extra = &parser.extra,
            .root_index = parser.root_node,
            .gpa = gpa,
            .declarations = undefined,
            .undeclared__ = undefined,
        };

        for (0..MAX_NESTING_LEVEL) |i| {
            v.declarations[i] = .{};
            v.undeclared__[i] = .{};
        }

        return v;
    }

    pub fn deinit(v: *SemanticValidator) void {
        for (0..MAX_NESTING_LEVEL) |i| {
            v.declarations[i].deinit(v.gpa);
            v.undeclared__[i].deinit(v.gpa);
        }
        v.errors.deinit(v.gpa);
    }

    fn enterBlock(v: *SemanticValidator) void {
        v.nesting_level += 1;
        if (v.nesting_level >= MAX_NESTING_LEVEL)
            return error.NestingLevelExceedsMaximum;
        // v.declarations[v.nesting_level].clearRetainingCapacity();
        // v.undeclared__[v.nesting_level].clearRetainingCapacity();
    }

    fn exitBlock(v: *SemanticValidator) void {
        std.debug.assert(v.nesting_level > 0);
        v.declarations[v.nesting_level].clearRetainingCapacity();
        v.undeclared__[v.nesting_level].clearRetainingCapacity();

        // TODO
        v.nesting_level -= 1;
    }

    fn registerDeclaration(v: *SemanticValidator, identifier_index: TokenIndex) !void {
        const token = v.tokens[identifier_index];
        std.debug.assert(token.tag == .IDENTIFIER);
        const varname = token.str(v.source);

        var error_occured = false;
        // check nesting levels above current one:
        for (0..v.nesting_level) |nl| {
            if (v.declarations[nl].get(varname)) |first_decl_token_idx| {
                // already declared => error
                try v.errors.append(v.gpa, .{ .decl_shadows_outer = .{
                    .outer_decl = first_decl_token_idx,
                    .error_decl = identifier_index,
                } });
                error_occured = true;
                break;
            }
        }

        if (!error_occured) {
            // check current nesting level:
            if (v.declarations[v.nesting_level].get(varname)) |first_decl_token_idx| {
                // already declared => error
                try v.errors.append(v.gpa, .{ .multi_decl_var = .{
                    .first_decl = first_decl_token_idx,
                    .error_decl = identifier_index,
                } });
            } else {
                try v.declarations[v.nesting_level].put(v.gpa, varname, identifier_index);
            }
        }
    }

    fn validateVariableUsage(v: *SemanticValidator, identifier_index: TokenIndex) !void {
        const token = v.tokens[identifier_index];
        std.debug.assert(token.tag == .IDENTIFIER);
        const varname = token.str(v.source);

        for (0..v.nesting_level + 1) |nl| {
            if (v.declarations[nl].contains(varname))
                return; // was declared. we are happy!
        }

        for (0..v.nesting_level + 1) |nl| {
            if (v.undeclared__[nl].contains(varname))
                return; // nothing to do this variable was already reported as undeclared.
        }

        // this variable apears the first time and is must be reported!
        try v.undeclared__[v.nesting_level].put(v.gpa, varname, identifier_index);
        try v.errors.append(v.gpa, .{ .undecl_var = identifier_index });
    }

    fn validateExpression(v: *SemanticValidator, node_idx: AstNodeIndex) SemanticValidatorException!void {
        const node = v.ast_nodes[node_idx];
        switch (node.tag) {
            .ATOM => {
                const token = v.tokens[node.token_index];
                if (token.tag == .IDENTIFIER)
                    try v.validateVariableUsage(node.token_index);
            },
            .FNCALL => try v.validateFnCall(node),
            .UNARY_OP => try v.validateExpression(node.lhs),
            .BINARY_OP => {
                try v.validateExpression(node.lhs);
                try v.validateExpression(node.rhs);
            },
            else => unreachable,
        }
    }

    fn validateFnCall(v: *SemanticValidator, node: AstNode) !void {
        const token = v.tokens[node.token_index];

        for (builtin_functions) |funcname| {
            if (!std.mem.eql(u8, funcname, token.str(v.source))) {
                try v.errors.append(v.gpa, .{ .unknown_function = node.token_index });
            }
            var argument_list = v.extra.getList(AstNodeIndex, node.rhs);
            while (argument_list.next()) |child_idx|
                try v.validateExpression(child_idx);
        }
    }

    fn validateAssignment(v: *SemanticValidator, node: AstNode) !void {
        std.debug.assert(node.tag == .ASSIGNMENT);
        const lhs_var_token_index = v.ast_nodes[node.lhs].token_index;
        const lhs_variable = v.tokens[lhs_var_token_index];
        // const lhs_varname = lhs_variable.str(v.source);
        std.debug.assert(lhs_variable.tag == .IDENTIFIER);

        try v.validateVariableUsage(lhs_var_token_index);

        try v.validateExpression(node.rhs);
    }

    fn validateDeclaration(v: *SemanticValidator, node: AstNode) !void {
        std.debug.assert(node.tag == .DECLARATION);
        const lhs_var_token_index = v.ast_nodes[node.lhs].token_index;
        const lhs_variable = v.tokens[lhs_var_token_index];
        // const lhs_varname = lhs_variable.str(v.source);
        std.debug.assert(lhs_variable.tag == .IDENTIFIER);

        try v.validateExpression(node.rhs);

        try v.registerDeclaration(lhs_var_token_index);
    }

    fn validateWhile(v: *SemanticValidator, node: AstNode) !void {
        try v.validateExpression(node.lhs);
        try v.validateBlock(node.rhs);
    }

    fn validateIf(v: *SemanticValidator, node: AstNode) !void {
        try v.validateExpression(node.condition());
        try v.validateBlock(node.then());
        try v.validateBlock(node.else_());
    }

    fn validateStatement(v: *SemanticValidator, node_idx: AstNodeIndex) !void {
        const node = v.ast_nodes[node_idx];
        switch (node.tag) {
            // zig fmt: off
            .ASSIGNMENT  => try v.validateAssignment(node),
            .DECLARATION => try v.validateDeclaration(node),
            .WHILE       => try v.validateWhile(node),
            .IF          => try v.validateIf(node),
            .FNCALL      => try v.validateFnCall(node),
            // zig fmt: on
            else => unreachable,
        }
    }

    fn validateBlock(v: *SemanticValidator, node_idx: AstNodeIndex) SemanticValidatorException!void {
        const node = v.ast_nodes[node_idx];
        std.debug.assert(node.tag == .BLOCK);
        var children = v.extra.getList(AstNodeIndex, node.rhs);
        while (children.next()) |child_idx| {
            try v.validateStatement(child_idx);
        }
    }

    pub fn validate(v: *SemanticValidator) !void {
        try v.validateBlock(v.root_index);

        // return error.NotImplemented;
        // for (v.statement_indices) |ci| {
        //     try v.validateStatement(ci);
        // }

        // if (!v.declared_variables.contains("result"))
        //     try v.errors.append(v.gpa, .{ .no_result = {} });
    }

    pub fn printErrors(v: *const SemanticValidator, writer: *std.Io.Writer, ts: TokenStream) !void {
        for (v.errors.items) |err| {
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
                .unknown_function => |token_idx| {
                    const pos = try ts.printLineAndMarkToken(writer, token_idx);
                    try writer.print(
                        "line {}: Error: unknown function \"{s}\".\n",
                        .{ pos.line, ts.sourceStr(token_idx) },
                    );
                    try writer.writeByte('\n');
                },
            }
        }
        try writer.flush();
    }
};

test "SemanticValidator" {
    const source =
        \\ x := -5.0 # declaring and assigning a variable
        \\ x = 2.0  # assigning an existing variable
        \\ d := 1
        \\ y := -x + -3 * - (-7 + -2) **-x + d
        \\ z := x ** y; p := 7.1
        \\ result := z + p**2
    ;

    var stdout_buff: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buff);
    const stdout = &stdout_writer.interface;

    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    std.debug.print("tokens: {}\n", .{ts.tokens.len});

    var parser = try Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();

    _ = try parser.parse();
    std.debug.print("ast nodes: {}\n", .{parser.ast_nodes.items.len});

    if (parser.errors.items.len > 0)
        try parser.printErrors(stdout, ts);

    // validate shit:
    var validator = SemanticValidator.init(gpa, &parser);
    defer validator.deinit();
    try validator.validate();
    try validator.printErrors(stdout, ts);
}
