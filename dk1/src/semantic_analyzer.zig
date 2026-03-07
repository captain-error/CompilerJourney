const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

const AstNode = par.AstNode;
const Parser = par.Parser;
const AstNodeIndex = par.AstNodeIndex;

pub const SemanticValidator = struct {
    source: []const u8,
    tokens: []const Token,
    ast_nodes: []const AstNode,
    command_indices: []const AstNodeIndex,
    gpa: std.mem.Allocator,

    // declared_variables: std.ArrayList(TokenIndex) = .empty,
    declared_variables: std.hash_map.StringHashMapUnmanaged(TokenIndex),
    undeclared_variables: std.hash_map.StringHashMapUnmanaged(TokenIndex),
    errors: std.ArrayList(Error) = .empty,

    pub const Error = union(enum) {
        undecl_var: TokenIndex,
        multi_decl_var: struct {
            first_decl: TokenIndex,
            error_decl: TokenIndex,
        },
        no_result: void,
    };

    pub fn init(
        gpa: std.mem.Allocator,
        source: []const u8,
        tokens: []const Token,
        ast_nodes: []const AstNode,
        command_indices: []const AstNodeIndex,
    ) SemanticValidator {
        return .{
            .source = source,
            .tokens = tokens,
            .ast_nodes = ast_nodes,
            .command_indices = command_indices,
            .gpa = gpa,
            .declared_variables = .{},
            .undeclared_variables = .{},
        };
    }

    pub fn deinit(v: *SemanticValidator) void {
        v.declared_variables.deinit(v.gpa);
        v.undeclared_variables.deinit(v.gpa);
        v.errors.deinit(v.gpa);
    }

    fn validateIdentifierUsage(v: *SemanticValidator, identifier_index: TokenIndex) !void {
        const token = v.tokens[identifier_index];
        const identifier_name = v.source[token.start..token.end];

        if (!v.declared_variables.contains(identifier_name)) { // is variable undeclared?
            // report undeclared variable, but only if it is the first report for this variable:
            if (!v.undeclared_variables.contains(identifier_name)) {
                try v.undeclared_variables.put(v.gpa, identifier_name, identifier_index);
                try v.errors.append(v.gpa, .{ .undecl_var = identifier_index });
            }
        }
    }

    fn validateExpression(v: *SemanticValidator, expression_index: AstNodeIndex) !void {
        const node = v.ast_nodes[expression_index];
        switch (node.tag) {
            .ATOM => {
                const token = v.tokens[node.token_index];
                if (token.tag == .IDENTIFIER)
                    try v.validateIdentifierUsage(node.token_index);
            },
            .UNARY_OP => try v.validateExpression(node.lhs),
            .BINARY_OP => {
                try v.validateExpression(node.lhs);
                try v.validateExpression(node.rhs);
            },
            else => unreachable,
        }
    }

    fn validateCommand(v: *SemanticValidator, command_index: AstNodeIndex) !void {
        const command = &v.ast_nodes[command_index];
        std.debug.assert(command.tag == .COMMAND);
        const lhs_var_token_index = v.ast_nodes[command.lhs].token_index;
        const lhs_variable = v.tokens[lhs_var_token_index];
        const lhs_varname = v.source[lhs_variable.start..lhs_variable.end];
        std.debug.assert(lhs_variable.tag == .IDENTIFIER);

        var declaration_to_add: ?[]const u8 = null;

        if (v.tokens[command.token_index].tag == .COLONEQ) {
            // declaration

            if (v.declared_variables.get(lhs_varname)) |first_decl_token_idx| {
                // already declared => error
                try v.errors.append(v.gpa, .{ .multi_decl_var = .{
                    .first_decl = first_decl_token_idx,
                    .error_decl = lhs_var_token_index,
                } });
            } else {
                // remember declaration:
                declaration_to_add = lhs_varname;
            }
        } else {
            // not a declaration
            try v.validateIdentifierUsage(lhs_var_token_index);
        }

        try v.validateExpression(command.rhs);

        if (declaration_to_add) |decl|
            try v.declared_variables.put(v.gpa, decl, lhs_var_token_index);
    }

    pub fn validate(v: *SemanticValidator) !void {
        for (v.command_indices) |ci| {
            try v.validateCommand(ci);
        }

        if (!v.declared_variables.contains("result"))
            try v.errors.append(v.gpa, .{ .no_result = {} });
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
                .no_result => {
                    try writer.writeAll("Error: No variable \"result\" declared. Please declare it. E.g.\nresult := some_value_or_calculation\n\n");
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

    var parser = Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();

    _ = try parser.parse();
    std.debug.print("ast nodes: {}\n", .{parser.ast_nodes.items.len});

    if (parser.errors.items.len > 0)
        try parser.printErrors(stdout, ts);

    // for (commands) |ast_index| {
    //     par.printAstBranch(parser, ast_index, 1);
    // }

    // validate shit:
    var validator = SemanticValidator.init(
        gpa,
        ts.source,
        ts.tokens,
        parser.ast_nodes.items,
        parser.top_level_commands.items,
    );
    defer validator.deinit();
    try validator.validate();
    try validator.printErrors(stdout, ts);
}
