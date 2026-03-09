const std = @import("std");
const tok = @import("tokenizer.zig");
const exdat = @import("extra_data.zig");

const Token = tok.Token;
const Tokenizer = tok.Tokenizer;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;
const ExtraData = exdat.ExtraData;
const ExtraIndex = exdat.ExtraIndex;

pub const AstNodeIndex = u32;
pub const AstNodeOffset = u16;
pub const PrecedenceLevel = u8;

const ParserError = error{
    OutOfMemory,
    IfConditionIsTooLong,
    InternalCompilerError,
};

pub const AstNode = struct {
    tag: Tag = .INVALID,
    token_index: TokenIndex = 0,

    lhs: AstNodeIndex = 0,
    rhs: AstNodeIndex = 0,
    offset: AstNodeOffset = 0,

    pub fn childCount(n: AstNode) usize {
        std.debug.assert(n.tag == .BLOCK or n.tag == .FNCALL);
        return n.lhs;
    }

    // pub fn childIndices(n: AstNode, extra: []AstNodeIndex) []AstNodeIndex {
    //     std.debug.assert(n.tag == .BLOCK);
    //     return extra[n.rhs .. n.rhs + n.lhs];
    // }

    pub fn firstChild(n: AstNode, extra: *const ExtraData) ExtraListElem {
        return extra.get(n.rhs).*;
    }

    pub fn condition(n: AstNode) AstNodeIndex {
        std.debug.assert(n.tag == .IF or n.tag == .WHILE);
        return n.lhs;
    }

    pub fn then(n: AstNode) AstNodeIndex {
        std.debug.assert(n.tag == .IF);
        if (n.offset == 0) return 0;
        return n.lhs + @as(AstNodeIndex, @intCast(n.offset));
    }

    pub fn setThen(n: *AstNode, then_idx: AstNodeIndex) !void {
        std.debug.assert(n.tag == .IF);
        std.debug.assert(n.lhs != 0);
        // std.debug.assert(then_idx > 0);
        if (then_idx == 0) {
            n.offset = 0;
            return;
        }
        const offset = then_idx - n.lhs;
        if (offset > 0xffff)
            return error.IfConditionIsTooLong;
        n.offset = @intCast(offset);
    }

    pub fn else_(n: AstNode) AstNodeIndex {
        std.debug.assert(n.tag == .IF);
        return n.rhs;
    }

    pub fn body(n: AstNode) AstNodeIndex {
        std.debug.assert(n.tag == .WHILE);
        return n.rhs;
    }

    const ExtraListElem = struct {
        node_idx: AstNodeIndex = 0,
        next: ExtraData.ExtraIndex = 0,

        fn nextSibling(el: ExtraListElem, extra: *const ExtraData) ExtraListElem {
            std.debug.assert(el.next != 0);
            std.debug.assert(el.next <= extra.data.items.len);
            return extra.get(el.next).*;
        }
    };

    pub const Tag = enum(u8) {
        INVALID = 0,
        DECLARE,
        COMMAND,
        BINARY_OP,
        UNARY_OP, // child at lhs, rhs == 0
        ATOM, // lhs == rhs == 0
        FNCALL, // single argument at lhs
        IF, // condition at lhs, then-part at lhs+offset, else-part at rhs
        WHILE,
        BLOCK, // lhs contains child count, rhs contains index into parser.extra
    };
};

pub const Parser = struct {
    source: []const u8,
    tokens: []const Token,
    ast_nodes: std.ArrayList(AstNode) = .empty,
    extra: ExtraData,
    // extra: std.ArrayList(AstNodeIndex) = .empty,
    // top_level_commands: std.ArrayList(AstNodeIndex) = .empty,
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
        var res = Parser{
            .ast_nodes = try .initCapacity(gpa, approximate_ast_node_count),
            .source = source,
            .tokens = tokens,
            .gpa = gpa,
            .token_idx = 1,
            .extra = try .init(gpa, approximate_ast_node_count),
        };

        res.ast_nodes.appendAssumeCapacity(.{}); // add empty invalid node
        return res;
    }

    pub fn deinit(p: *Parser) void {
        p.ast_nodes.deinit(p.gpa);
        p.extra.deinit();
        p.errors.deinit(p.gpa);
    }

    fn peak(p: *const Parser, lookahead: u32) Token {
        const idx = @min(p.token_idx + lookahead, p.tokens.len - 1);
        return p.tokens[idx];
    }

    fn next(p: *Parser) void {
        if (p.token_idx >= p.tokens.len - 1)
            return;

        p.token_idx += 1;
    }

    fn addNode(p: *Parser, node: AstNode) !AstNodeIndex {
        // std.debug.print("addNode: {any} token={any}\n", .{ node, p.tokens[node.token_index] });
        const index: AstNodeIndex = @intCast(p.ast_nodes.items.len);
        try p.ast_nodes.append(p.gpa, node);
        return index;
    }

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

    fn advanceTillCommandEndOnErr(p: *Parser, val: InternalParserError!AstNodeIndex) ParserError!AstNodeIndex {
        if (val) |v| {
            return v;
        } else |err| {
            switch (err) {
                error.UnexpectedToken => {
                    p.advanceTillCommandEnd();
                    return 0;
                },
                else => |e| return e,
            }
        }
    }

    pub fn advanceTillCommandEnd(p: *Parser) void {
        while (true) {
            switch (p.peak(0).tag) {
                .EOF, .EOL, .SEMICOLON => return,
                else => p.next(),
            }
        }
    }

    pub fn advanceTillBlockEnd(p: *Parser) void {
        while (true) {
            switch (p.peak(0).tag) {
                .EOF, .END => return,
                else => p.next(),
            }
        }
    }

    fn precedenceOf(p: *const Parser, token_index: TokenIndex) PrecedenceLevel {
        return switch (p.tokens[token_index].tag) {
            .GT, .GE, .LT, .LE => 1,
            .PLUS, .MINUS => 2,
            .TIMES, .DIV => 3,
            .POW => 4,
            else => 0,
        };
    }

    fn isRightAssociative(tag: Token.Tag) bool {
        _ = tag;
        return false; // FIXME!!!
    }

    fn expectToken(p: *Parser, tag: Token.Tag) ParserError!bool {
        if (p.peak(0).tag != tag) {
            try p.emitError(p.token_idx, switch (tag) {
                inline else => |t| @tagName(t),
            });
            return false;
        }

        return true;
    }

    fn expectOneOf(p: *Parser, comptime token_tags: anytype, expetation_msg: [:0]const u8) ParserError!bool {
        const actual_tag = p.peak(0).tag;
        // std.debug.print("expectOneOf({any}): actual: {any}\n", .{ token_tags, actual_tag });
        inline for (token_tags) |tag| {
            if (actual_tag == tag)
                return true;
        }
        try p.emitError(p.token_idx, expetation_msg);
        return false;
    }

    fn parseAtom(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseAtom: start: {s}\n", .{@tagName(p.peak(0).tag)});

        if (!try p.expectOneOf(
            .{
                .FLOAT_LIT,
                .INT_LIT,
                .TRUE,
                .FALSE,
                .IDENTIFIER,
            },
            "literal or variable",
        )) return error.UnexpectedToken;

        const idx = p.token_idx;
        p.next();

        return p.addNode(.{
            .token_index = idx,
            .tag = .ATOM,
        });
    }

    fn parseExpression(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseExpression: start: {s}\n", .{@tagName(p.peak(0).tag)});
        return p.parseExpression1(1);
    }

    fn parseExpression1(p: *Parser, min_precedence: PrecedenceLevel) InternalParserError!AstNodeIndex {
        // std.debug.print("parseExpression1: {} start: {s}\n", .{ min_precedence, @tagName(p.peak(0).tag) });

        var lhs: AstNodeIndex = undefined;
        if (p.peak(0).tag == .MINUS) {
            p.next();

            lhs = try p.addNode(.{
                .tag = .UNARY_OP,
                .token_index = p.token_idx,
                .lhs = try p.parseSubExpr(),
            });
        } else {
            lhs = try p.parseSubExpr();
        }

        while (true) {
            if (p.peak(0).tag == .RPAREN) break;
            if (p.peak(0).tag == .EOF) break;

            const op_precedence = p.precedenceOf(p.token_idx); // will return 0 if the token is not an operator
            // std.debug.print("parseExpression1: {} op: {s} precedence: {} \n", .{ min_precedence, @tagName(p.peak(0).tag), op_precedence });
            if (op_precedence < min_precedence) break; // min_precedence is always > 0.

            const op = p.token_idx;
            p.next();

            const rhs = try p.parseExpression1(op_precedence);

            lhs = try p.addNode(.{
                .tag = .BINARY_OP,
                .token_index = op,
                .lhs = lhs,
                .rhs = rhs,
            });
        } // outer while
        return lhs;
    } // fn

    fn parseSubExpr(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseSubExpr: start: {s}\n", .{@tagName(p.peak(0).tag)});
        var res: AstNodeIndex = undefined;
        if (p.peak(0).tag == .LPAREN) {
            p.next();
            res = try p.parseExpression1(1);
            if (!try p.expectToken(.RPAREN)) return error.UnexpectedToken;
            p.next();
        } else {
            res = try p.parseAtom();
        }
        return res;
    }

    fn parseFunCall(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peak(0).tag == .IDENTIFIER);
        const name_token = p.token_idx;
        p.next();

        std.debug.assert(p.peak(0).tag == .LPAREN);
        p.next();

        var param_list = p.extra.startList(AstNodeIndex);

        while (p.peak(0).tag != .RPAREN) {
            try param_list.append(try p.parseExpression());
            if (p.peak(0).tag != .COMMA) break;
            p.next();
        }

        if (!try p.expectToken(.RPAREN)) return error.UnexpectedToken;
        p.next();

        return p.addNode(.{
            .tag = .FNCALL,
            .token_index = name_token,
            .lhs = param_list.count,
            .rhs = param_list.start_index,
        });
    }

    /// in case of parse error consumes all tokens till .EOL or .SEMICOLON and returns 0
    fn parseCommand(p: *Parser) ParserError!AstNodeIndex {
        const variable = try p.addNode(.{
            .tag = .ATOM,
            .token_index = p.token_idx,
        });

        if (!try p.expectToken(.IDENTIFIER)) {
            p.advanceTillCommandEnd();
            return 0;
        }
        p.next();

        if (!try p.expectOneOf(.{
            .DECLARE,
            .ASSIGN,
            .PLUSASSIGN,
            .MINUSASSIGN,
        }, "assignement or declaration operator")) {
            p.advanceTillCommandEnd();
            return 0;
        }

        const op = p.token_idx;
        p.next();

        const expr = p.parseExpression() catch |err| switch (err) {
            error.UnexpectedToken => {
                p.advanceTillCommandEnd();
                return 0;
            },
            else => |e| return e,
        };

        if (!try p.expectOneOf(.{
            .SEMICOLON,
            .EOL,
            .EOF,
        }, "semicolon or end-of-line")) {
            p.advanceTillCommandEnd();
            return 0;
        }
        p.next();

        return try p.addNode(.{
            .tag = .COMMAND,
            .token_index = op,
            .lhs = variable,
            .rhs = expr,
        });
    }

    pub fn parseWhile(p: *Parser) ParserError!AstNodeIndex {
        std.debug.assert(p.peak(0).tag == .WHILE);
        var res = AstNode{ .token_index = p.token_idx, .tag = .WHILE };
        p.next();

        res.lhs = try p.advanceTillCommandEndOnErr(p.parseExpression());

        if (!try p.expectToken(.EOL)) {
            p.advanceTillCommandEnd();
            return 0;
        }
        p.next();

        res.rhs = try p.parseBlock();
        if (!try p.expectToken(.END)) {
            // p.advanceTillBlockEnd();
            // p.next();
            return 0;
        }
        p.next();

        return try p.addNode(res);
    }

    pub fn parseIf(p: *Parser) ParserError!AstNodeIndex {
        std.debug.assert(p.peak(0).tag == .IF);
        var res = AstNode{ .token_index = p.token_idx, .tag = .IF };
        p.next();

        res.lhs = try p.advanceTillCommandEndOnErr(p.parseExpression());

        if (!try p.expectToken(.EOL)) {
            p.advanceTillCommandEnd();
            return 0;
        }
        p.next();

        try res.setThen(try p.parseBlock());
        if (p.peak(0).tag == .ELSE) {
            p.next();

            res.rhs = try p.parseBlock();
        }

        if (!try p.expectToken(.END)) {
            p.advanceTillBlockEnd();
            p.next();
            return 0;
        }
        p.next();

        return try p.addNode(res);
    }

    // does not consume .END!
    pub fn parseBlock(p: *Parser) ParserError!AstNodeIndex {
        const token_idx = p.token_idx;

        var child_list = p.extra.startList(AstNodeIndex);

        loop: while (true) {
            const token = p.peak(0);
            // std.debug.print("#### {} {s}[{s}]\n", .{ p.token_idx, @tagName(token.tag), token.str(p.source) });
            switch (token.tag) {
                .EOF, .END, .ELSE => break :loop,
                .EOL, .SEMICOLON => p.next(), // ignore
                // zig fmt: off
                .IF         => try child_list.append(try p.parseIf()     ),
                .WHILE      => try child_list.append(try p.parseWhile()  ),
                .IDENTIFIER => try child_list.append(try p.parseCommand()),
                // zig fmt: on
                else => {
                    try p.emitError(p.token_idx, "UNEXPECTED!");
                    p.advanceTillBlockEnd();
                    p.next();
                    return 0;
                },
            } // switch token
        }

        return p.addNode(.{
            .token_index = token_idx,
            .tag = .BLOCK,
            .lhs = child_list.count,
            .rhs = child_list.start_index, // index of first child-index
        });
    }

    pub fn parse(p: *Parser) ParserError!void {
        // p.token_idx = 1;
        p.root_node = try p.parseBlock();
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

        const node = p.ast_nodes.items[ast_index];
        const token = p.tokens[node.token_index];

        try writer.print("{s} ({s}[{s}])    ", .{
            @tagName(node.tag),
            @tagName(token.tag),
            if (token.tag == .EOL) "\\n" else token.str(p.source),
        });

        if (node.tag == .BLOCK or node.tag == .FNCALL) {
            try writer.print(" #children= {}\n", .{node.childCount()});
            var child_list = p.extra.getList(AstNodeIndex, node.rhs);
            while (child_list.next()) |i|
                try p.printAstBranch(writer, i, indentation + 1);

            return;
        }
        try writer.writeByte('\n');

        if (node.tag == .ATOM) return;
        try p.printAstBranch(writer, node.lhs, indentation + 1);

        if (node.tag == .UNARY_OP) return;
        try p.printAstBranch(writer, node.rhs, indentation + 1);
    }
};

fn testParser(source: []const u8, parse_func: fn (parser: *Parser) Parser.InternalParserError!AstNodeIndex, print_always: bool) !void {
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
        for (parser.ast_nodes.items) |node| {
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
        return error.NotAllTokensUsed;
    }

    if (ast_idx == 0 or parser.hasErrors() or print_always) {
        try stdout.writeAll("All tokens:\n");
        try ts.prettyPrintTokens(stdout);
        try stdout.writeAll("-----------------------\n\n");

        if (ast_idx == 0) {
            try stdout.writeAll("--- all AST nodes:\n");
            for (parser.ast_nodes.items) |node| {
                const token = ts.tokens[node.token_index];
                try stdout.print("{s} ({s}[{s}])) ", .{ @tagName(node.tag), @tagName(token.tag), token.str(source) });
            }
            try stdout.writeAll("\n---------------\n");
        } else {
            try stdout.writeAll("AST:\n");
            try parser.printAstBranch(stdout, ast_idx, 1);
        }

        if (!print_always) {
            try stdout.print("\nPARSER REPORTED ERRORS:\n", .{});
            try parser.printErrors(stdout, ts);
            return error.ParserReportedErrors;
        }
    }

    // try ts.prettyPrintTokens(stdout);
    // try stdout.writeAll("-----------------------\n\n");

    // try parser.printErrors(stdout, ts);

    // try stdout.writeAll("\n\nAST:\n");
    // try parser.printAstBranch(stdout, ast_idx, 1);

    // try stdout.writeAll("\n--- all AST nodes:\n");
    // for (parser.ast_nodes.items) |node| {
    //     const token = ts.tokens[node.token_index];
    //     try stdout.print("{s} ({s}[{s}]) ", .{ @tagName(node.tag), @tagName(token.tag), if (token.tag == .EOL) "\\n" else token.str(source) });
    // }
    // try stdout.writeAll("\n---------------\n");
}

test "parse expression" {
    const source = "a < 2";
    try testParser(source, Parser.parseExpression, false);
}

test "parse expression 2" {
    const source = "(a < 2) * 6";
    try testParser(source, Parser.parseExpression, false);
}

test "parse expression 3" {
    const source = "(a < 2) * B + (2)";
    try testParser(source, Parser.parseExpression, false);
}

test "parse expression 4" {
    const source = "-a < 2";
    try testParser(source, Parser.parseExpression, false);
}

test "just arithmetic" {
    const source =
        \\ x := -5.0 # declaring and assigning a variable
        \\ x = 2.0  # assigning an existing variable
        \\ y := -x + -3 * - (-7 + -2) **-x
        \\ z := x ** y; p := 7.1
        \\ result := z + p**2
    ;

    try testParser(source, Parser.parseBlock, false);
}

test "while" {
    const source =
        \\ while a
        \\  x += 2.0
        \\ end
    ;
    try testParser(source, Parser.parseWhile, false);
}

test "if" {
    const source =
        \\ if a
        \\  x += 2.0
        \\ end
    ;
    try testParser(source, Parser.parseIf, false);
}

test "if else" {
    const source =
        \\ if a
        \\   x += 2.0
        \\ else
        \\   x -= 5.1
        \\ end
    ;
    try testParser(source, Parser.parseIf, false);
}

test "nested if else" {
    const source =
        \\ if a
        \\      if b < 2
        \\          x += 2.0
        \\      else
        \\          x -= 1.2
        \\      end
        \\ else
        \\      if x > 7.1
        \\          x -= 5.1
        \\      end
        \\ end
    ;
    try testParser(source, Parser.parseIf, false);
}

test "parse function call" {
    const source = "doStuff()";
    try testParser(source, Parser.parseFunCall, false);
}

test "parse function call wit 1 param" {
    const source = "doStuff(a)";
    try testParser(source, Parser.parseFunCall, false);
}

test "parse function call wit 1 param + trailing comma" {
    const source = "doStuff(a,)";
    try testParser(source, Parser.parseFunCall, false);
}

test "parse function call wit 2 params" {
    const source = "doStuff(a, b)";
    try testParser(source, Parser.parseFunCall, false);
}

test "parse function call wit 2 params + trailing comma" {
    const source = "doStuff(a, b,)";
    try testParser(source, Parser.parseFunCall, true);
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
//     // std.debug.print("ast nodes: {}\n", .{parser.ast_nodes.items.len});
//     if (parser.hasErrors()) {
//         try parser.printErrors(stdout, ts);
//     }
//     // try stdout.writeAll("AST:\n");
//     // try parser.printAstBranch(stdout, parser.root_node, 1);
//     // try stdout.flush();
// }
