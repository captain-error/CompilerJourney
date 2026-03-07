const std = @import("std");
const tok = @import("tokenizer.zig");

const Token = tok.Token;
const Tokenizer = tok.Tokenizer;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

pub const AstNodeIndex = usize;
pub const PrecedenceLevel = u8;

pub const AstNode = struct {
    tag: Tag = .INVALID,
    token_index: TokenIndex = 0,
    lhs: AstNodeIndex = 0,
    rhs: AstNodeIndex = 0,

    pub const Tag = enum(u8) {
        INVALID = 0,
        DECLARE,
        COMMAND,
        BINARY_OP,
        UNARY_OP,
        ATOM,
    };
};

pub const Parser = struct {
    source: []const u8,
    tokens: []const Token,
    ast_nodes: std.ArrayList(AstNode) = .empty,
    top_level_commands: std.ArrayList(AstNodeIndex) = .empty,
    errors: std.ArrayList(Error) = .empty,
    gpa: std.mem.Allocator,

    tok_i: TokenIndex = 0,

    pub const Error = struct {
        token_index: TokenIndex,
        expected: [:0]const u8,
    };

    pub const ParserError = error{
        UnexpectedToken,
    };

    pub fn init(source: []const u8, tokens: []const Token, gpa: std.mem.Allocator) Parser {
        std.debug.assert(tokens.len > 0);
        var res = Parser{
            .source = source,
            .tokens = tokens,
            .gpa = gpa,
        };

        res.ast_nodes.append(gpa, .{}) catch unreachable;
        return res;
    }

    pub fn deinit(p: *Parser) void {
        p.ast_nodes.deinit(p.gpa);
        p.top_level_commands.deinit(p.gpa);
        p.errors.deinit(p.gpa);
    }

    fn addNode(p: *Parser, node: AstNode) AstNodeIndex {
        const index = p.ast_nodes.items.len;
        p.ast_nodes.append(p.gpa, node) catch unreachable;
        return index;
    }

    fn eatToken(p: *Parser, tag: Token.Tag) ?TokenIndex {
        const i = p.tok_i;
        if (p.tokens[i].tag != tag) return null;
        p.tok_i += 1;
        return i;
    }

    fn expectToken(p: *Parser, tag: Token.Tag) ParserError!TokenIndex {
        if (p.tokens[p.tok_i].tag != tag) {
            p.emitError(p.tok_i, switch (tag) {
                inline else => |t| @tagName(t),
            });
            return ParserError.UnexpectedToken;
        }
        const res = p.tok_i;
        p.tok_i += 1;
        return res;
    }

    fn emitError(p: *Parser, token_index: TokenIndex, expected: [:0]const u8) void {
        const err = p.errors.addOne(p.gpa) catch unreachable;
        err.token_index = token_index;
        err.expected = expected;
    }

    fn expectOneOf(p: *Parser, comptime token_tags: anytype, expetation_msg: [:0]const u8) ParserError!TokenIndex {
        const actual_tag = p.tokens[p.tok_i].tag;
        inline for (token_tags) |tag| {
            if (actual_tag == tag) {
                const res = p.tok_i;
                if (tag != .EOF) // never consume EOF
                    p.tok_i += 1;
                return res;
            }
        }
        p.emitError(p.tok_i, expetation_msg);
        return error.UnexpectedToken;
    }

    fn parseAtom(p: *Parser) !AstNodeIndex {
        // const res = p.eatToken(.IDENTIFIER) orelse p.eatToken(.NUM) orelse 0;
        // if (res == 0) {
        //     p.emitError(p.tok_i, "number or variable");
        //     return error.UnexpectedToken;
        // }
        const token_index = try p.expectOneOf(
            .{ .NUM, .IDENTIFIER },
            "number or variable",
        );
        return p.addNode(.{
            .token_index = token_index,
            .tag = .ATOM,
        });
    }

    pub fn advanceTillCommandEnd(p: *Parser) void {
        while (true) {
            switch (p.tokens[p.tok_i].tag) {
                .EOF, .EOL, .SEMICOLON => return,
                else => p.tok_i += 1,
            }
        }
    }

    pub fn parse(p: *Parser) ![]AstNodeIndex {
        p.tok_i = 1;
        while (p.tokens[p.tok_i].tag != .EOF) {
            switch (p.tokens[p.tok_i].tag) {
                .EOL, .SEMICOLON => p.tok_i += 1, // ignore
                .IDENTIFIER => {
                    if (p.parseCommand()) |command| {
                        p.top_level_commands.append(p.gpa, command) catch unreachable;
                    } else |_| {
                        p.advanceTillCommandEnd();
                    }
                },
                else => {
                    p.emitError(p.tok_i, "identifier");
                    p.advanceTillCommandEnd();
                },
            }
        }
        if (p.errors.items.len > 0)
            return error.ParsingFailed;
        return p.top_level_commands.items;
    }

    fn parseCommand(p: *Parser) !AstNodeIndex {
        const variable = p.addNode(.{
            .tag = .ATOM,
            .token_index = try p.expectToken(.IDENTIFIER),
        });

        const op =
            p.eatToken(.COLONEQ) orelse
            p.eatToken(.EQ) orelse
            p.eatToken(.PLUSEQ) orelse
            p.eatToken(.MINUSEQ) orelse 0;

        if (op == 0) {
            p.emitError(p.tok_i, "assignment operator");
            return error.UnexpectedToken;
        }

        const expr = try p.parseExpression();
        _ = try p.expectOneOf(.{ .SEMICOLON, .EOL, .EOF }, "semicolon or end-of-line");

        return p.addNode(.{
            .tag = .COMMAND,
            .token_index = op,
            .lhs = variable,
            .rhs = expr,
        });
    }

    fn precedenceOf(p: *const Parser, token_index: TokenIndex) PrecedenceLevel {
        return switch (p.tokens[token_index].tag) {
            .PLUS, .MINUS => 1,
            .STAR, .SLASH => 2,
            .STARSTAR => 3,
            else => 0,
        };
    }

    fn isRightAssociative(tag: Token.Tag) bool {
        _ = tag;
        return false; // FIXME!!!
    }

    fn parseExpression(p: *Parser) !AstNodeIndex {
        // std.debug.print("parseExpression: start: {s}\n", .{@tagName(p.tokens[p.tok_i].tag)});
        return p.parseExpression1(1);
    }

    fn parseSubExpr(p: *Parser) !AstNodeIndex {
        var res: AstNodeIndex = undefined;
        if (p.eatToken(.LPAREN)) |_| {
            res = try p.parseExpression1(1);
            _ = try p.expectToken(.RPAREN);
        } else {
            res = try p.parseAtom();
        }
        return res;
    }

    fn parseExpression1(p: *Parser, min_precedence: PrecedenceLevel) ParserError!AstNodeIndex {
        var lhs: AstNodeIndex = undefined;
        if (p.eatToken(.MINUS)) |token_index| {
            lhs = p.addNode(.{
                .tag = .UNARY_OP,
                .token_index = token_index,
                .lhs = try p.parseSubExpr(),
            });
        } else {
            lhs = try p.parseSubExpr();
        }

        while (true) {
            if (p.tokens[p.tok_i].tag == .RPAREN) break;

            const op_precedence = p.precedenceOf(p.tok_i); // will return 0 if the token is not an operator
            if (op_precedence < min_precedence) break; // min_precedence is always > 0.

            const op = p.tok_i;
            p.tok_i += 1;

            const rhs = try p.parseExpression1(op_precedence);

            lhs = p.addNode(.{
                .tag = .BINARY_OP,
                .token_index = op,
                .lhs = lhs,
                .rhs = rhs,
            });
        } // outer while
        return lhs;
    } // fn

    pub fn printErrors(p: *const Parser, writer: *std.Io.Writer, ts: TokenStream) !void {
        for (p.errors.items) |err| {
            const pos = try ts.printLineAndMarkToken(writer, err.token_index);
            const token = ts.tokens[err.token_index];
            try writer.print("line {}: Error: found {s}. Expected: {s}\n\n", .{ pos.line, @tagName(token.tag), err.expected });
        }
        try writer.flush();
    }
};

pub fn printAstBranch(p: Parser, root_index: AstNodeIndex, indentation: usize) void {
    if (root_index == 0) return;

    const node = p.ast_nodes.items[root_index];
    const token = p.tokens[node.token_index];
    for (0..indentation) |_|
        std.debug.print("    ", .{});
    std.debug.print("{s} [{s}({s})]\n", .{
        @tagName(node.tag),
        @tagName(token.tag),
        p.source[token.start..token.end],
    });
    // std.debug.print("{s:>{}}{s} [{s}({s})]", .{
    //     "",
    //     4 * indentation,
    //     @tagName(node.tag),
    //     @tagName(token.tag),
    //     p.source[token.start..token.end],
    // });

    printAstBranch(p, node.lhs, indentation + 1);
    printAstBranch(p, node.rhs, indentation + 1);
}

test "Parser" {
    const source =
        \\ x := -5.0 # declaring and assigning a variable
        \\ x = 2.0  # assigning an existing variable
        \\ y := -x + -3 * - (-7 + -2) **-x
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

    if (parser.parse()) |commands| {
        std.debug.print("ast nodes: {}\n", .{parser.ast_nodes.items.len});
        for (commands) |ast_index| {
            // printAstBranch(parser, ast_index, 1);
            _ = ast_index;
        }
    } else |_| {
        try parser.printErrors(stdout, ts);
    }
}
