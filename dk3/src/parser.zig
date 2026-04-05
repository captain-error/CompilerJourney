const std = @import("std");
const tok = @import("tokenizer.zig");
const treemod = @import("tree.zig");
// const exdat = @import("extra_data.zig");

const assert = std.debug.assert;

const Token = tok.Token;
const Tokenizer = tok.Tokenizer;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

pub const AST = treemod.GenericTree(AstNode);
pub const AstNodeIndex = treemod.NodeIndex;

pub const AstNode = struct {
    tag: AstNodeTag = .INVALID,
    token_index: TokenIndex = 0,

    first_child: AstNodeIndex = 0,
    next_sibling: AstNodeIndex = 0,

    pub fn hasChild(node: AstNode) bool {
        return node.first_child > 0;
    }

    pub fn children(node: AstNode, ast: *const AST) AST.LinkedList {
        return ast.getList(node.first_child);
    }

    // convenience methods for certain node kinds:
    pub const ConditionThenElse = struct { cond_idx: AstNodeIndex, then_idx: AstNodeIndex, else_idx: AstNodeIndex };
    pub fn conditionThenElse(node: AstNode, ast: *const AST) ConditionThenElse {
        assert(node.tag == .IF or node.tag == .WHILE);

        const cond_idx = node.first_child;
        const then_idx = ast.get(cond_idx).next_sibling;
        const else_idx = ast.get(then_idx).next_sibling;

        assert(cond_idx != 0);
        assert(then_idx != 0);

        return .{ .cond_idx = cond_idx, .then_idx = then_idx, .else_idx = else_idx };
    }

    pub fn then(node: AstNode, ast: *const AST) AstNodeIndex {
        assert(node.tag == .IF);
        const then_idx = ast.get(node.first_child).next_sibling;
        assert(then_idx != 0);
        return then_idx;
    }
};

pub const AstNodeTag = enum(u8) {
    INVALID = 0,
    DECLARATION, // 2 children: lhs, rhs
    ASSIGNMENT, // 2 children: lhs, rhs
    BINARY_OP, // 2 children: lhs, rhs
    UNARY_OP, // 1 child
    FNCALL, // N children for N params
    WHILE, // 2 children: condition
    BLOCK, // arbitrary many children
    ATOM, // no child
    IF, // 3 children: condition, then, else
};

pub const PrecedenceLevel = u8;

const ParserError = error{
    OutOfMemory,
    IfConditionIsTooLong,
    InternalCompilerError,
};

pub const Parser = struct {
    source: []const u8,
    tokens: []const Token,
    ast: AST,
    root_node: AstNodeIndex = 0,
    errors: std.ArrayList(Error) = .empty,
    gpa: std.mem.Allocator,

    token_idx: TokenIndex,

    pub fn hasErrors(p: *const Parser) bool {
        return p.errors.items.len > 0;
    }

    pub const Error = struct {
        token_index: TokenIndex,
        expected: [:0]const u8,
    };

    const InternalParserError = error{
        OutOfMemory,
        IfConditionIsTooLong,
        UnexpectedToken, // only used internally,
        InternalCompilerError,
    };

    pub fn init(source: []const u8, tokens: []const Token, gpa: std.mem.Allocator) !Parser {
        std.debug.assert(tokens.len > 0);
        const approximate_ast_node_count = tokens.len + 1; // TODO: revise this
        return .{
            .ast = try .init(gpa, approximate_ast_node_count),
            .source = source,
            .tokens = tokens,
            .gpa = gpa,
            .token_idx = 1,
        };
    }

    pub fn deinit(p: *Parser) void {
        p.ast.deinit();
        p.errors.deinit(p.gpa);
    }

    fn peek(p: *const Parser, lookahead: u32) Token {
        const idx = @min(p.token_idx + lookahead, p.tokens.len - 1);
        return p.tokens[idx];
    }

    fn next(p: *Parser) void {
        if (p.token_idx >= p.tokens.len - 1)
            return;

        p.token_idx += 1;
    }

    // fn addNode(p: *Parser, node: AstNode) !AstNodeIndex {
    //     // std.debug.print("addNode: {any} token={any}\n", .{ node, p.tokens[node.token_index] });
    //     const index: AstNodeIndex = @intCast(p.ast.nodes.items.len);
    //     try p.ast_nodes.append(p.gpa, node);
    //     return index;
    // }

    // fn addExtra(p: *Parser, node_index: AstNodeIndex) !void {
    //     try p.extra.append(p.gpa, node_index);
    // }

    fn emitError(p: *Parser, token_index: TokenIndex, expected: [:0]const u8) !void {
        const err = try p.errors.addOne(p.gpa);
        err.token_index = token_index;
        err.expected = expected;
    }

    fn filterErr(val: InternalParserError!AstNodeIndex) ParserError!AstNodeIndex {
        return val catch |err| switch (err) {
            error.UnexpectedToken => return 0, // add INVALID ast node. continue parsing.
            else => |e| return e,
        };
    }

    fn advanceTillEoStatementOnErr(p: *Parser, val: InternalParserError!AstNodeIndex) ParserError!AstNodeIndex {
        if (val) |v| {
            return v;
        } else |err| {
            switch (err) {
                error.UnexpectedToken => {
                    p.advanceTillEoStatement();
                    return 0;
                },
                else => |e| return e,
            }
        }
    }

    pub fn advanceTillEoStatement(p: *Parser) void {
        while (true) {
            switch (p.peek(0).tag) {
                .EOF, .EOL, .SEMICOLON => return,
                else => p.next(),
            }
        }
    }

    pub fn advanceTillEoBlock(p: *Parser) void {
        while (true) {
            switch (p.peek(0).tag) {
                .EOF, .END => return,
                else => p.next(),
            }
        }
    }

    fn precedenceOf(p: *const Parser, token_index: TokenIndex) PrecedenceLevel {
        return switch (p.tokens[token_index].tag) {
            .OR => 1,
            .XOR => 2,
            .AND => 3,
            .GT, .GE, .LT, .LE => 4,
            .PLUS, .MINUS => 5,
            .TIMES, .DIV => 6,
            .POW => 7,
            else => 0,
        };
    }

    fn isRightAssociative(tag: Token.Tag) bool {
        _ = tag;
        return false; // FIXME!!!
    }

    fn expectToken(p: *Parser, tag: Token.Tag) ParserError!bool {
        if (p.peek(0).tag != tag) {
            try p.emitError(p.token_idx, switch (tag) {
                inline else => |t| @tagName(t),
            });
            return false;
        }

        return true;
    }

    fn expectOneOf(p: *Parser, comptime token_tags: anytype, expetation_msg: [:0]const u8) ParserError!bool {
        const actual_tag = p.peek(0).tag;
        // std.debug.print("expectOneOf({any}): actual: {any}\n", .{ token_tags, actual_tag });
        inline for (token_tags) |tag| {
            if (actual_tag == tag)
                return true;
        }
        try p.emitError(p.token_idx, expetation_msg);
        return false;
    }

    fn expectEndOfStatement(p: *Parser) ParserError!bool {
        switch (p.peek(0).tag) {
            .END_BLOCK, .EOL, .SEMICOLON, .EOF => return true,
            else => {
                try p.emitError(p.token_idx, "semicolon or end-of-line");
                return false;
            },
        }
    }

    // fn parseAtom(p: *Parser) InternalParserError!AstNodeIndex {
    //     // std.debug.print("parseAtom: start: {s}\n", .{@tagName(p.peek(0).tag)});

    //     if (!try p.expectOneOf(
    //         .{
    //             .FLOAT_LIT,
    //             .INT_LIT,
    //             .TRUE,
    //             .FALSE,
    //             .IDENTIFIER,
    //         },
    //         "literal or variable",
    //     )) return error.UnexpectedToken;

    //     const idx = p.token_idx;
    //     p.next();

    //     return p.addNode(.{
    //         .token_index = idx,
    //         .tag = .ATOM,
    //     });
    // }

    fn parseAtomOrFuncall(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseAtomOrFuncall: start: {s}\n", .{@tagName(p.peek(0).tag)});

        if (!try p.expectOneOf(
            .{
                .FLOAT_LIT,
                .INT_LIT,
                .TRUE,
                .FALSE,
                .IDENTIFIER,
            },
            "literal, variable or function call",
        )) return error.UnexpectedToken;

        if (p.peek(0).tag == .IDENTIFIER and p.peek(1).tag == .LPAREN)
            return p.parseFunCall();

        const idx = p.token_idx;
        p.next();

        return p.ast.append(.{
            .token_index = idx,
            .tag = .ATOM,
        });
    }

    fn parseExpression(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseExpression: start: {s}\n", .{@tagName(p.peek(0).tag)});
        return p.parseExpression1(1);
    }

    fn parseExpression1(p: *Parser, min_precedence: PrecedenceLevel) InternalParserError!AstNodeIndex {
        // std.debug.print("parseExpression1: {} start: {s}\n", .{ min_precedence, @tagName(p.peek(0).tag) });

        var lhs: AstNodeIndex = undefined;
        switch (p.peek(0).tag) {
            .MINUS, .NOT => {
                lhs = try p.ast.append(.{
                    .tag = .UNARY_OP,
                    .token_index = p.token_idx,
                });
                p.next();
                p.ast.get(lhs).first_child = try p.parseSubExpr();
            },
            else => lhs = try p.parseSubExpr(),
        }

        while (true) {
            if (p.peek(0).tag == .RPAREN) break;
            if (p.peek(0).tag == .EOF) break;

            const op_precedence = p.precedenceOf(p.token_idx); // will return 0 if the token is not an operator
            // std.debug.print("parseExpression1: {} op: {s} precedence: {} \n", .{ min_precedence, @tagName(p.peek(0).tag), op_precedence });
            if (op_precedence < min_precedence) break; // min_precedence is always > 0.

            const op = p.token_idx;
            p.next();

            const binop_idx = try p.ast.append(.{
                .tag = .BINARY_OP,
                .token_index = op,
                .first_child = lhs,
            });

            const rhs = try p.parseExpression1(op_precedence);
            p.ast.get(lhs).next_sibling = rhs;

            lhs = binop_idx;
        } // outer while
        return lhs;
    } // fn

    fn parseSubExpr(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseSubExpr: start: {s}\n", .{@tagName(p.peek(0).tag)});
        var res: AstNodeIndex = undefined;
        if (p.peek(0).tag == .LPAREN) {
            p.next();
            res = try p.parseExpression1(1);
            if (!try p.expectToken(.RPAREN)) return error.UnexpectedToken;
            p.next();
        } else {
            res = try p.parseAtomOrFuncall();
        }
        return res;
    }

    fn parseFunCall(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .IDENTIFIER);
        const name_token = p.token_idx;
        p.next();

        std.debug.assert(p.peek(0).tag == .LPAREN);
        p.next();

        const node_idx = try p.ast.append(.{
            .tag = .FNCALL,
            .token_index = name_token,
        });

        var param_list = p.ast.startList();

        while (p.peek(0).tag != .RPAREN) {
            try param_list.appendExisting(try p.parseExpression());
            if (p.peek(0).tag != .COMMA) break;
            p.next();
        }

        if (!try p.expectToken(.RPAREN)) return error.UnexpectedToken;
        p.next();

        p.ast.get(node_idx).first_child = param_list.start_index;

        return node_idx;
    }

    /// in case of parse error consumes all tokens till .EOL or .SEMICOLON and returns 0
    fn parserDeclOrAssign(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .IDENTIFIER);

        // reserve space:
        const node_idx = try p.ast.append(.{});

        const variable_idx = try p.ast.append(.{
            .tag = .ATOM,
            .token_index = p.token_idx,
        });

        // if (!try p.expectToken(.IDENTIFIER))
        //     return error.UnexpectedToken;
        p.next();

        if (!try p.expectOneOf(.{
            .DECLARE,
            .ASSIGN,
            .PLUSASSIGN,
            .MINUSASSIGN,
            .MULTASSIGN,
            .DIVASSIGN,
        }, "assignement or declaration operator"))
            return error.UnexpectedToken;

        const op = p.token_idx;
        p.next();

        const rhs = try p.parseExpression();

        if (!try p.expectEndOfStatement())
            return error.UnexpectedToken;
        p.next();

        const node = p.ast.get(node_idx);
        node.* = .{
            .tag = if (p.tokens[op].tag == .DECLARE) .DECLARATION else .ASSIGNMENT,
            .token_index = op,
            .first_child = variable_idx,
        };

        p.ast.get(variable_idx).next_sibling = rhs;

        return node_idx;
    }

    pub fn parseWhile(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .WHILE);
        const node_idx = try p.ast.append(.{ .token_index = p.token_idx, .tag = .WHILE });
        p.next();

        const condition_idx = try p.parseExpression();

        if (!try p.expectToken(.EOL))
            return error.UnexpectedToken;
        p.next();

        const body_idx = try p.parseIndentedBlock();

        p.ast.get(node_idx).first_child = condition_idx;
        p.ast.get(condition_idx).next_sibling = body_idx;

        return node_idx;
    }

    pub fn parseIf(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.assert(p.peek(0).tag == .IF);
        const node_idx = try p.ast.append(.{ .token_index = p.token_idx, .tag = .IF });
        p.next();

        const condition_idx = try p.parseExpression();

        if (!try p.expectToken(.EOL))
            return error.UnexpectedToken;
        p.next();

        const then_idx = try p.parseIndentedBlock();

        var else_idx: AstNodeIndex = 0;
        if (p.peek(0).tag == .ELSE) {
            p.next();
            if (!try p.expectToken(.EOL))
                return error.UnexpectedToken;
            p.next();

            else_idx = try p.parseIndentedBlock();
        }

        p.ast.get(node_idx).first_child = condition_idx;
        p.ast.get(condition_idx).next_sibling = then_idx;
        if (else_idx > 0)
            p.ast.get(then_idx).next_sibling = else_idx;

        return node_idx;
    }

    pub fn parseStatement(p: *Parser) ParserError!AstNodeIndex {
        // std.debug.print("####parseStatement: {} {s}[{s}]\n", .{ p.token_idx, @tagName(p.peek(0).tag), p.peek(0).str(p.source) });
        var res: InternalParserError!AstNodeIndex = 0;
        switch (p.peek(0).tag) {
            // zig fmt: off
            .IF         => res = p.parseIf(),
            .WHILE      => res = p.parseWhile(),
            // zig fmt: on
            .IDENTIFIER => {
                if (p.peek(1).tag == .LPAREN) {
                    res = p.parseFunCall();
                    if (res) |_| {
                        if (try p.expectEndOfStatement()) {
                            p.next();
                        } else {
                            res = error.UnexpectedToken;
                        }
                    } else |_| {}
                } else {
                    res = p.parserDeclOrAssign();
                }
            },
            else => {
                try p.emitError(p.token_idx, "statement");
                res = error.UnexpectedToken;
            },
        }

        return res catch |err| switch (err) {
            error.UnexpectedToken => {
                p.advanceTillEoStatement();
                p.next();
                return 0;
            },
            else => |e| e,
        };
    }

    pub fn parseIndentedBlock(p: *Parser) ParserError!AstNodeIndex {
        if (!try p.expectToken(.BEGIN_BLOCK))
            return try p.ast.append(.{
                .token_index = p.token_idx,
                .tag = .BLOCK,
            }); // empty block
        p.next();

        const block_idx = try p.parseBlock();

        if (!try p.expectToken(.END_BLOCK))
            return block_idx; // continue parsing despite error
        p.next();

        return block_idx;
    }

    // does not consume .BEGIN_BLOCK and .END_BLOCK!
    pub fn parseBlock(p: *Parser) ParserError!AstNodeIndex {
        assert(p.peek(0).tag != .BEGIN_BLOCK);

        const token_idx = p.token_idx;
        const block_node_idx = try p.ast.append(.{
            .token_index = token_idx,
            .tag = .BLOCK,
        });

        var child_list = p.ast.startList();

        loop: while (true) {
            const token = p.peek(0);
            // std.debug.print("#### {} {s}[{s}]\n", .{ p.token_idx, @tagName(token.tag), token.str(p.source) });
            switch (token.tag) {
                // zig fmt: off
                .END_BLOCK, .EOF => break :loop,
                .EOL, .SEMICOLON  => p.next(), // ignore

                else => {
                    // std.debug.print("token.tag={}\n", .{ @tagName(token.tag) });
                    const node_idx = try p.parseStatement();
                    try child_list.appendExisting(node_idx);
                },
                // zig fmt: on

            } // switch token
        }

        p.ast.get(block_node_idx).first_child = child_list.start_index;
        return block_node_idx;
    }

    pub fn parse(p: *Parser) ParserError!AstNodeIndex {
        p.root_node = if (p.peek(0).tag == .BEGIN_BLOCK) try p.parseIndentedBlock() else try p.parseBlock();
        return p.root_node;
    }

    pub fn printErrors(p: *const Parser, writer: *std.Io.Writer, ts: TokenStream) !void {
        for (p.errors.items) |err| {
            const pos = try ts.printLineAndMarkToken(writer, err.token_index);
            const token = ts.tokens[err.token_index];
            try writer.print("line {}: Error: found {s}. Expected: {s}\n\n", .{ pos.line, @tagName(token.tag), err.expected });
        }
        try writer.flush();
    }

    pub fn printAstBranch(p: *const Parser, writer: *std.Io.Writer, ast_index: AstNodeIndex, indentation: usize) !void {
        try writer.splatByteAll(' ', 4 * indentation);

        if (ast_index == 0) {
            try writer.writeAll("INVALID\n");
            return;
        }

        const node = p.ast.get(ast_index).*;
        const token = p.tokens[node.token_index];

        try writer.print("{s} ({s}[{s}]) #{} (next_sibling={})\n", .{
            @tagName(node.tag),
            @tagName(token.tag),
            if (token.tag == .EOL) "" else token.str(p.source),
            ast_index,
            p.ast.get(ast_index).next_sibling,
        });

        if (node.hasChild()) {
            var child_list = node.children(&p.ast);
            while (child_list.nextIdx()) |child_idx|
                try p.printAstBranch(writer, child_idx, indentation + 1);
        }
        // if (node.tag == .BLOCK or node.tag == .FNCALL) {
        //     try writer.print(" #children= {}\n", .{node.childCount()});
        //     var child_list = p.extra.getList(AstNodeIndex, node.rhs);
        //     while (child_list.next()) |i|
        //         try p.printAstBranch(writer, i, indentation + 1);

        //     return;
        // }
        // try writer.writeByte('\n');

        // if (node.tag == .ATOM) return;
        // try p.printAstBranch(writer, node.lhs, indentation + 1);

        // if (node.tag == .UNARY_OP) return;
        // try p.printAstBranch(writer, node.rhs, indentation + 1);
    }

    pub fn printAllNodesFlat(p: *const Parser, writer: *std.Io.Writer) !void {
        for (p.ast.nodes.items, 0..) |node, idx| {
            const token = p.tokens[node.token_index];
            try writer.print("{s} ({s}[{s}]))#{} ", .{
                @tagName(node.tag),
                @tagName(token.tag),
                token.str(p.source),
                idx,
            });
        }
        try writer.writeByte('\n');
    }
};

fn testAstStructure(ast: *const AST) void {
    for (1..ast.nodes.items.len) |i| {
        const idx: AstNodeIndex = @intCast(i);
        const node = ast.get(idx).*;
        const num_kids = ast.countChildrenOf(idx);
        switch (node.tag) {
            // zig fmt: off
            .INVALID     => unreachable,
            .ASSIGNMENT  => assert(num_kids == 2),
            .DECLARATION => assert(num_kids == 2),
            .BINARY_OP   => assert(num_kids == 2),
            .ATOM        => assert(num_kids == 0),
            .IF          => assert(num_kids == 2 or num_kids == 3),
            .WHILE       => assert(num_kids == 2),
            .UNARY_OP    => assert(num_kids == 1),
            .BLOCK       => {},
            .FNCALL      => {},
            // zig fmt: on
        }
    }
}

test "parse expression" {
    const source = "a < 2";
    const expected = .{
        AstNodeTag.BINARY_OP,                            Token.Tag.LT,                                 "<",
        .{ AstNodeTag.ATOM, Token.Tag.IDENTIFIER, "a" }, .{ AstNodeTag.ATOM, Token.Tag.INT_LIT, "2" },
    };
    try testParser(source, Parser.parseExpression, expected, false);
}

test "parse expression 2" {
    const source = "(a < 2) * 6";
    const expected = .{
        AstNodeTag.BINARY_OP, Token.Tag.TIMES, "*",
        .{ AstNodeTag.BINARY_OP, Token.Tag.LT, "<", .{ AstNodeTag.ATOM, Token.Tag.IDENTIFIER, "a" }, .{ AstNodeTag.ATOM, Token.Tag.INT_LIT, "2" } }, // a < 2
        .{ AstNodeTag.ATOM, Token.Tag.INT_LIT, "6" },
    };
    try testParser(source, Parser.parseExpression, expected, false);
}

test "parse expression 3" {
    const source = "(a < 2) * B + (2)";
    const expected = .{
        AstNodeTag.BINARY_OP, Token.Tag.PLUS, "+",
        .{ // (a < 2) * B
            AstNodeTag.BINARY_OP, Token.Tag.TIMES, "*",
            .{ AstNodeTag.BINARY_OP, Token.Tag.LT, "<", .{ AstNodeTag.ATOM, "a" }, .{ AstNodeTag.ATOM, "2" } }, // a < 2
            .{ AstNodeTag.ATOM, "B" },
        },
        .{ AstNodeTag.ATOM, "2" }, // (2)
    };
    try testParser(source, Parser.parseExpression, expected, false);
}

test "parse expression 4" {
    const source = "-a < 2";
    const expected = .{
        AstNodeTag.BINARY_OP, Token.Tag.LT, "<",
        .{ AstNodeTag.UNARY_OP, Token.Tag.MINUS, "-", .{ AstNodeTag.ATOM, "a" } }, // -a
        .{ AstNodeTag.ATOM, "2" },
    };
    try testParser(source, Parser.parseExpression, expected, false);
}

test "just arithmetic" {
    const source =
        \\ x := -5.0 # declaring and assigning a variable
        \\ x = 2.0  # assigning an existing variable
        \\ y := -x + -3 * - (-7 + -2) **-x
        \\ z := x ** y; p := 7.1
        \\ result := z + p**2
    ;
    const expected = .{
        AstNodeTag.BLOCK, // whole block
        .{ AstNodeTag.DECLARATION, Token.Tag.DECLARE, ":=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.UNARY_OP, Token.Tag.MINUS, "-", .{ AstNodeTag.ATOM, "5.0" } } }, // x := -5.0
        .{ AstNodeTag.ASSIGNMENT, Token.Tag.ASSIGN, "=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "2.0" } }, // x = 2.0
        .{ // y := -x + -3 * - (-7 + -2) **-x
            AstNodeTag.DECLARATION, ":=", .{ AstNodeTag.ATOM, "y" }, //
            .{
                AstNodeTag.BINARY_OP, Token.Tag.PLUS, "+", //
                .{ AstNodeTag.UNARY_OP, "-", .{ AstNodeTag.ATOM, "x" } }, //
                .{
                    AstNodeTag.BINARY_OP, Token.Tag.TIMES, "*", //
                    .{ AstNodeTag.UNARY_OP, Token.Tag.MINUS, "-", .{ AstNodeTag.ATOM, "3" } }, //
                    .{
                        AstNodeTag.BINARY_OP, Token.Tag.POW, "**", //
                        .{
                            AstNodeTag.UNARY_OP, Token.Tag.MINUS, "-", .{
                                AstNodeTag.BINARY_OP,                                     Token.Tag.PLUS,                                           "+",
                                .{ AstNodeTag.UNARY_OP, "-", .{ AstNodeTag.ATOM, "7" } }, .{ AstNodeTag.UNARY_OP, "-", .{ AstNodeTag.ATOM, "2" } },
                            },
                        },
                        .{ AstNodeTag.UNARY_OP, "-", .{ AstNodeTag.ATOM, "x" } },
                    },
                },
            },
        },
        .{
            AstNodeTag.DECLARATION, ":=", .{ AstNodeTag.ATOM, "z" }, //
            .{
                AstNodeTag.BINARY_OP,      Token.Tag.POW,             "**", //
                .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "y" },
            },
        }, // z := x ** y
        .{ AstNodeTag.DECLARATION, ":=", .{ AstNodeTag.ATOM, "p" }, .{ AstNodeTag.ATOM, "7.1" } }, // p := 7.1
        .{
            AstNodeTag.DECLARATION, ":=", .{ AstNodeTag.ATOM, "result" }, //
            .{
                AstNodeTag.BINARY_OP,      Token.Tag.PLUS, "+", //
                .{ AstNodeTag.ATOM, "z" },
                .{
                    AstNodeTag.BINARY_OP,      Token.Tag.POW,             "**", //
                    .{ AstNodeTag.ATOM, "p" }, .{ AstNodeTag.ATOM, "2" },
                },
            },
        }, // result := z + p**2
    };

    try testParser(source, Parser.parseIndentedBlock, expected, false);
}

test "simple while" {
    const source =
        \\while a
        \\  x += 2.0
        \\
    ;
    const expected = .{
        AstNodeTag.WHILE, // while a
        .{ AstNodeTag.ATOM, "a" }, // condition
        .{ // body:
            AstNodeTag.BLOCK,
            .{
                AstNodeTag.ASSIGNMENT,
                Token.Tag.PLUSASSIGN,
                "+=",
                .{ AstNodeTag.ATOM, Token.Tag.IDENTIFIER, "x" },
                .{ AstNodeTag.ATOM, Token.Tag.FLOAT_LIT, "2.0" },
            },
        },
    };
    try testParser(source, Parser.parseWhile, expected, false);
}

test "complex while" {
    const source =
        \\while a < 20
        \\   x += 2.0
        \\   b = 7
        \\
    ;
    const expected = .{
        AstNodeTag.WHILE, // while a < 20
        .{ AstNodeTag.BINARY_OP, Token.Tag.LT, "<", .{ AstNodeTag.ATOM, "a" }, .{ AstNodeTag.ATOM, "20" } }, // condition
        .{ // body:
            AstNodeTag.BLOCK,
            .{ AstNodeTag.ASSIGNMENT, Token.Tag.PLUSASSIGN, "+=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "2.0" } },
            .{ AstNodeTag.ASSIGNMENT, Token.Tag.ASSIGN, "=", .{ AstNodeTag.ATOM, "b" }, .{ AstNodeTag.ATOM, "7" } },
        },
    };
    try testParser(source, Parser.parseWhile, expected, false);
}

test "if" {
    const source =
        \\if a
        \\  x += 2.0
    ;
    const expected = .{
        AstNodeTag.IF, // if a
        .{ AstNodeTag.ATOM, "a" }, // condition
        .{ // then:
            AstNodeTag.BLOCK,
            .{ AstNodeTag.ASSIGNMENT, Token.Tag.PLUSASSIGN, "+=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "2.0" } },
        },
    };
    try testParser(source, Parser.parseIf, expected, false);
}

test "if else" {
    const source =
        \\if a
        \\   x += 2.0
        \\else
        \\   x -= 5.1
    ;
    const expected = .{
        AstNodeTag.IF, // if a
        .{ AstNodeTag.ATOM, "a" }, // condition
        .{ // then:
            AstNodeTag.BLOCK,
            .{ AstNodeTag.ASSIGNMENT, Token.Tag.PLUSASSIGN, "+=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "2.0" } },
        },
        .{ // else:
            AstNodeTag.BLOCK,
            .{ AstNodeTag.ASSIGNMENT, Token.Tag.MINUSASSIGN, "-=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "5.1" } },
        },
    };
    try testParser(source, Parser.parseIf, expected, false);
}

test "nested if else" {
    const source =
        \\if a
        \\     if b < 2
        \\         x += 2.0
        \\     else
        \\         x -= 1.2
        \\
        \\else
        \\     if x > 7.1
        \\         x -= 5.1
    ;
    const expected = .{
        AstNodeTag.IF, // if a
        .{ AstNodeTag.ATOM, "a" }, // condition
        .{ // then:
            AstNodeTag.BLOCK,
            .{
                AstNodeTag.IF, // if b < 2
                .{ AstNodeTag.BINARY_OP, Token.Tag.LT, "<", .{ AstNodeTag.ATOM, "b" }, .{ AstNodeTag.ATOM, "2" } }, // condition
                .{ // then:
                    AstNodeTag.BLOCK,
                    .{ AstNodeTag.ASSIGNMENT, Token.Tag.PLUSASSIGN, "+=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "2.0" } },
                },
                .{ // else:
                    AstNodeTag.BLOCK,
                    .{ AstNodeTag.ASSIGNMENT, "-=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "1.2" } },
                },
            },
        },
        .{ // else:
            AstNodeTag.BLOCK,
            .{
                AstNodeTag.IF, // if x > 7.1
                .{ AstNodeTag.BINARY_OP, ">", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "7.1" } }, // condition
                .{ // then:
                    AstNodeTag.BLOCK,
                    .{ AstNodeTag.ASSIGNMENT, "-=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "5.1" } },
                },
            },
        },
    };
    try testParser(source, Parser.parseIf, expected, false);
}

test "parse function call" {
    const source = "doStuff()";
    const expected = .{ AstNodeTag.FNCALL, "doStuff" };
    try testParser(source, Parser.parseFunCall, expected, false);
}

test "parse function call wit 1 param" {
    const source = "doStuff(a)";
    const expected = .{ AstNodeTag.FNCALL, "doStuff", .{ AstNodeTag.ATOM, "a" } };
    try testParser(source, Parser.parseFunCall, expected, false);
}

test "parse function call wit 1 param + trailing comma" {
    const source = "doStuff(a,)";
    const expected = .{ AstNodeTag.FNCALL, "doStuff", .{"a"} };
    try testParser(source, Parser.parseFunCall, expected, false);
}

test "parse function call wit 2 params" {
    const source = "doStuff(a, b)";
    const expected = .{ AstNodeTag.FNCALL, "doStuff", .{"a"}, .{"b"} };
    try testParser(source, Parser.parseFunCall, expected, false);
}

test "parse function call wit 2 params + trailing comma" {
    const source = "doStuff(a, b,)";
    const expected = .{ AstNodeTag.FNCALL, "doStuff", .{"a"}, .{"b"} };
    try testParser(source, Parser.parseFunCall, expected, false);
}

test "parse function with parser.parse()" {
    const source = "doStuff(a, b,)";
    const expected = .{ AstNodeTag.BLOCK, .{ AstNodeTag.FNCALL, "doStuff", .{"a"}, .{"b"} } };
    try testParser(source, Parser.parse, expected, false);
}

test "parse complex program" {
    const source =
        \\ jahr := 0
        \\ zins := 1.02
        \\ result := 1.0
        \\ while jahr < 10
        \\      result *= zins
        \\      jahr += 1
        \\ 
        \\ print(result)
    ;
    try testParser(source, Parser.parse, .{}, false);
}

test "AST vs reference" {
    const src = "x := y + z";
    // parse src, get the AST, and compare it to the expected structure:
    const expected = .{
        AstNodeTag.DECLARATION,
        .{ AstNodeTag.ATOM, "x" },
        .{
            AstNodeTag.BINARY_OP,
            "+",
            .{ AstNodeTag.ATOM, "y" },
            .{ AstNodeTag.ATOM, "z" },
        },
    };
    try testParser(src, Parser.parserDeclOrAssign, expected, false);
}

/// Returns true if the passed type will coerce to []const u8.
/// Any of the following are considered strings:
/// ```
/// []const u8, [:S]const u8, *const [N]u8, *const [N:S]u8,
/// []u8, [:S]u8, *[:S]u8, *[N:S]u8.
/// ```
/// These types are not considered strings:
/// ```
/// u8, [N]u8, [*]const u8, [*:0]const u8,
/// [*]const [N]u8, []const u16, []const i8,
/// *const u8, ?[]const u8, ?*const [N]u8.
/// ```
pub fn isZigString(comptime T: type) bool {
    return comptime blk: {
        // Only pointer types can be strings, no optionals

        const info = @typeInfo(T);
        if (info != .pointer) break :blk false;

        const ptr = &info.pointer;
        // Check for CV qualifiers that would prevent coerction to []const u8

        if (ptr.is_volatile or ptr.is_allowzero) break :blk false;

        // If it's already a slice, simple check.

        if (ptr.size == .slice) {
            break :blk ptr.child == u8;
        }

        // Otherwise check if it's an array type that coerces to slice.

        if (ptr.size == .one) {
            const child = @typeInfo(ptr.child);
            if (child == .array) {
                const arr = &child.array;
                break :blk arr.child == u8;
            }
        }

        break :blk false;
    };
}

fn expectAstStructure(
    ast: *const AST,
    ts: *const TokenStream,
    writer: *std.Io.Writer,
    ast_idx: AstNodeIndex,
    nesting_depth: usize, // only used for error reporting
    expected: anytype,
) !void {
    if (ast_idx == 0) {
        try writer.splatByteAll(' ', nesting_depth * 4);
        try writer.print("ast_idx is 0. Expected: {any}\n", .{expected});
        return error.WrongNodeType;
    }

    if (expected.len == 0)
        return; // nothing to expect, ignore everything.

    const node = ast.get(ast_idx).*;
    const token = ts.tokens[node.token_index];
    var child_list = node.children(ast);

    inline for (expected) |e| {
        if (@TypeOf(e) == @TypeOf(null)) {
            return; // ignore the rest, everything before matched!
        } else if (@TypeOf(e) == AstNodeTag) {
            if (node.tag != e) {
                try writer.splatByteAll(' ', nesting_depth * 4);
                try writer.print("expected AST node {s} but found {s} ({s}[{s}])\n", .{ @tagName(e), @tagName(node.tag), @tagName(token.tag), if (token.tag == .EOL) "\\n" else token.str(ts.source) });
                return error.WrongNodeType;
            }
        } else if (@TypeOf(e) == Token.Tag) {
            if (token.tag != e) {
                try writer.splatByteAll(' ', nesting_depth * 4);
                try writer.print("expected token {s} but found {s} ({s}[{s}])\n", .{ @tagName(e), @tagName(token.tag), @tagName(token.tag), if (token.tag == .EOL) "\\n" else token.str(ts.source) });
                return error.WrongTokenTag;
            }
        } else {
            const e_type_info = @typeInfo(@TypeOf(e));
            switch (e_type_info) {
                .array => unreachable,
                .pointer => |p| {
                    // @compileLog(e, p, p.child, @typeInfo(p.child));
                    assert(p.size == .one);
                    // @compileLog(e, p.child, @typeInfo(p.child));
                    assert(@typeInfo(p.child) == .array);
                    assert(@typeInfo(p.child).array.child == u8);
                    const expected_token_str = e;
                    if (!std.mem.eql(u8, token.str(ts.source), expected_token_str)) {
                        try writer.splatByteAll(' ', nesting_depth * 4);
                        try writer.print("expected token string {s} but found {s}\n", .{ expected_token_str, token.str(ts.source) });
                        return error.StringMismatch;
                    }
                },
                .@"struct" => {
                    assert(e_type_info.@"struct".is_tuple);
                    // recursively check child:
                    const expected_child = e;
                    if (child_list.nextIdx()) |child_idx| {
                        expectAstStructure(ast, ts, writer, child_idx, nesting_depth + 1, expected_child) catch |err| {
                            try writer.splatByteAll(' ', nesting_depth * 4);
                            try writer.print("{s} ({s}[{s}]))\n", .{ @tagName(node.tag), @tagName(token.tag), if (token.tag == .EOL) "\\n" else token.str(ts.source) });
                            return err;
                        };
                    } else {
                        // report that expected child is missing.
                        try writer.splatByteAll(' ', nesting_depth * 4);
                        try writer.print("child missing. Expected: {any}\n", .{expected_child});
                        return error.ChildMissing;
                    }
                },
                else => unreachable,
            } // switch e_type_info
        }
    } // for expected

    if (child_list.nextIdx()) |extra_child_idx| {
        // report that there are more children than expected.
        try writer.splatByteAll(' ', nesting_depth * 4);
        try writer.print("extra child found: AST idx: {}", .{extra_child_idx});
        try writer.print("({s})\n", .{@tagName(ast.get(extra_child_idx).tag)});
        return error.ExtraChild;
    }

    return; // everything matched!
}

// fn (parser: *Parser) Parser.InternalParserError!AstNodeIndex
fn testParser(source: []const u8, parse_func: anytype, expected_structure: anytype, print_always: bool) !void {
    const gpa = std.testing.allocator;

    var stdout_buff: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buff);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch unreachable;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var parser = try Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();
    const ast_idx = parse_func(&parser) catch |err| {
        try stdout.writeAll("All tokens:\n");
        try ts.prettyPrintTokens(stdout);
        try stdout.writeAll("-----------------------\n\n");

        try parser.printErrors(stdout, ts);

        // try stdout.writeAll("\n\nAST:\n");
        // try parser.printAstBranch(stdout, parser.root_node, 1);

        try stdout.writeAll("--- all AST nodes:\n");
        for (parser.ast.nodes.items) |node| {
            const token = ts.tokens[node.token_index];
            try stdout.print("{s} ({s}[{s}])) ", .{ @tagName(node.tag), @tagName(token.tag), token.str(source) });
        }
        try stdout.writeAll("\n---------------\n");

        return err;
    };
    if (ts.tokens[parser.token_idx].tag != .EOF) {
        try stdout.writeAll("All tokens:\n");
        try ts.prettyPrintTokens(stdout);
        try stdout.writeAll("-----------------------\n\n");

        try stdout.writeAll("AST:\n");
        try parser.printAstBranch(stdout, ast_idx, 1);

        try stdout.print("\nNOT ALL TOKENS USED.\nunused tokens:\n", .{});
        for (ts.tokens[parser.token_idx..]) |t|
            try stdout.print("{s}[{s}] ", .{ @tagName(t.tag), t.str(source) });
        try stdout.print("\n\n", .{});
        testAstStructure(&parser.ast);
        return error.NotAllTokensUsed;
    }

    if (ast_idx == 0 or parser.hasErrors() or print_always) {
        try stdout.writeAll("All tokens:\n");
        try ts.prettyPrintTokens(stdout);
        try stdout.writeAll("-----------------------\n\n");

        if (ast_idx == 0) {
            try stdout.writeAll("--- all AST nodes:\n");
            for (parser.ast.nodes.items) |node| {
                const token = ts.tokens[node.token_index];
                try stdout.print("{s} ({s}[{s}])) ", .{ @tagName(node.tag), @tagName(token.tag), token.str(source) });
            }
            try stdout.writeAll("\n---------------\n");
        } else {
            try stdout.writeAll("AST:\n");
            try parser.printAstBranch(stdout, ast_idx, 1);
        }

        if (parser.hasErrors()) {
            try stdout.print("\nPARSER REPORTED ERRORS:\n", .{});
            try parser.printErrors(stdout, ts);
            return error.ParserReportedErrors;
        }
    }

    // testAstStructure(&parser.ast);
    expectAstStructure(&parser.ast, &ts, stdout, ast_idx, 0, expected_structure) catch |err| {
        try stdout.writeAll("Writing complete AST because of non-matching structure:\n");
        try parser.printAstBranch(stdout, ast_idx, 1);

        return err;
    };

    // try ts.prettyPrintTokens(stdout);
    // try stdout.writeAll("-----------------------\n\n");

    // try parser.printErrors(stdout, ts);

    // try stdout.writeAll("\n\nAST:\n");
    // try parser.printAstBranch(stdout, ast_idx, 1);

    // try stdout.writeAll("\n--- all AST nodes:\n");
    // for (parser.ast.nodes.items) |node| {
    //     const token = ts.tokens[node.token_index];
    //     try stdout.print("{s} ({s}[{s}]) ", .{ @tagName(node.tag), @tagName(token.tag), if (token.tag == .EOL) "\\n" else token.str(source) });
    // }
    // try stdout.writeAll("\n---------------\n");
    try stdout.flush();
}

// test "Parser" {
//     const source =
//         \\ x := -5.0 # declaring and assigning a variable
//         \\ x = 2.0  # assigning an existing variable
//         \\ y := -x + -3 * - (-7 + -2) **-x
//         \\ z := x ** y; p := 7.1
//         \\ result := z + p**2
//     ;

//     var stdout_buff: [1024]u8 = undefined;
//     var stdout_writer = std.fs.File.stdout().writer(&stdout_buff);
//     const stdout = &stdout_writer.interface;

//     const gpa = std.testing.allocator;

//     var ts = try TokenStream.init(source, gpa);
//     defer ts.deinit(gpa);

//     // std.debug.print("tokens: {}\n", .{ts.tokens.len});

//     var parser = try Parser.init(source, ts.tokens, gpa);
//     defer parser.deinit();
//     try parser.parse();
//     // std.debug.print("ast nodes: {}\n", .{parser.ast.nodes.items.len});
//     if (parser.hasErrors()) {
//         try parser.printErrors(stdout, ts);
//     }
//     // try stdout.writeAll("AST:\n");
//     // try parser.printAstBranch(stdout, parser.root_node, 1);
//     // try stdout.flush();
// }
