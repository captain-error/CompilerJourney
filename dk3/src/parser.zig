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

    pub fn parseAtomOrFuncall(p: *Parser) InternalParserError!AstNodeIndex {
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

    pub fn parseExpression(p: *Parser) InternalParserError!AstNodeIndex {
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

    pub fn parseSubExpr(p: *Parser) InternalParserError!AstNodeIndex {
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

    pub fn parseFunCall(p: *Parser) InternalParserError!AstNodeIndex {
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
    pub fn parserDeclOrAssign(p: *Parser) InternalParserError!AstNodeIndex {
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
