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
const AstNodeTag = par.AstNodeTag;

fn testAstStructure(ast: *const AST) void {
    for (1..ast.nodes.items.len) |i| {
        const idx: AstNodeIndex = @intCast(i);
        const node = ast.get(idx).*;
        const num_kids = ast.countChildrenOf(idx);
        switch (node.tag) {
            // zig fmt: off
            .INVALID          => unreachable,
            .ASSIGNMENT       => assert(num_kids == 2),
            .DECLARATION      => assert(num_kids >= 1 and num_kids <= 2), // [optional TYPE, optional rhs_expr]
            .BINARY_OP        => assert(num_kids == 2),
            .ATOM             => assert(num_kids == 0),
            .IF               => assert(num_kids == 2 or num_kids == 3),
            .WHILE            => assert(num_kids == 2),
            .UNARY_OP         => assert(num_kids == 1),
            .BLOCK            => {},
            .CALL_OR_INST     => {},
            .NAMED_ARG        => assert(num_kids == 1),
            .FNPARAMS         => {},
            .RETURN           => assert(num_kids == 1),
            .FNDECL           => assert(num_kids == 2),
            .STRUCTDECL       => {},
            .STRUCT_MEMBER_BARE => assert(num_kids == 0),
            .TYPE             => assert(num_kids == 0),
            .MEMBER_ACCESS    => assert(num_kids == 1),
            // zig fmt: on
        }
    }
}

test "parse function declaration" {
    const source =
        \\fn getAnswer()
        \\  return 42
    ;
    const expected = .{
        AstNodeTag.FNDECL, "getAnswer", .{AstNodeTag.FNPARAMS}, //
        .{ AstNodeTag.BLOCK, .{
            AstNodeTag.RETURN,
            Token.Tag.RETURN,
            "return",
            .{ AstNodeTag.ATOM, Token.Tag.INT_LIT, "42" },
        } },
    };
    try testParser(source, Parser.parseFnDecl, expected, false);
}

test "parse function declaration 2" {
    const source =
        \\fn add(a, b)
        \\  return a + b
    ;
    const expected = .{
        AstNodeTag.FNDECL, "add",
        .{
            AstNodeTag.FNPARAMS,
            .{ AstNodeTag.ATOM, Token.Tag.IDENTIFIER, "a" },
            .{ AstNodeTag.ATOM, Token.Tag.IDENTIFIER, "b" },
        },
        .{
            AstNodeTag.BLOCK, .{
                AstNodeTag.RETURN, Token.Tag.RETURN, "return", .{
                    AstNodeTag.BINARY_OP, Token.Tag.PLUS, "+",
                    .{ AstNodeTag.ATOM, Token.Tag.IDENTIFIER, "a" }, //
                    .{ AstNodeTag.ATOM, Token.Tag.IDENTIFIER, "b" },
                },
            },
        },
    };
    try testParser(source, Parser.parseFnDecl, expected, false);
}

test "parse mini program" {
        const source =
        \\fn main()
        \\    jahr := 0
        \\    zins := 1.02
        \\    result := 1.0
        \\    while jahr < 10
        \\         result *= zins
        \\         jahr += 1
        \\    
        \\    print(result)
    ;

    const expected = .{
        AstNodeTag.BLOCK, .{
            AstNodeTag.FNDECL, "main", .{AstNodeTag.FNPARAMS}, .{
                AstNodeTag.BLOCK,
                .{ AstNodeTag.DECLARATION, "jahr", .{ AstNodeTag.ATOM, "0" } },
                .{ AstNodeTag.DECLARATION, "zins", .{ AstNodeTag.ATOM, "1.02" } },
                .{ AstNodeTag.DECLARATION, "result", .{ AstNodeTag.ATOM, "1.0" } },
                .{
                    AstNodeTag.WHILE,
                    .{ AstNodeTag.BINARY_OP, "<", .{ AstNodeTag.ATOM, "jahr" }, .{ AstNodeTag.ATOM, "10" } },
                    .{
                        AstNodeTag.BLOCK,
                        .{ AstNodeTag.ASSIGNMENT, "*=", .{ AstNodeTag.ATOM, "result" }, .{ AstNodeTag.ATOM, "zins" } },
                        .{ AstNodeTag.ASSIGNMENT, "+=", .{ AstNodeTag.ATOM, "jahr" }, .{ AstNodeTag.ATOM, "1" } },
                    },
                },
                .{ AstNodeTag.CALL_OR_INST, "print", .{ AstNodeTag.ATOM, "result" } },
            },
        },
    };
    try testParser(source, Parser.parse, expected, false);
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


test "correct operator associativity a - b + c" {
    const source =
        \\a - b + c
    ;
    
    const expected = .{
        AstNodeTag.BINARY_OP, Token.Tag.PLUS, "+",
        .{ AstNodeTag.BINARY_OP, Token.Tag.MINUS, "-", 
            .{ AstNodeTag.ATOM, "a" }, 
            .{ AstNodeTag.ATOM, "b" },
         },
        .{ AstNodeTag.ATOM, "c" },
    };

    try testParser(source, Parser.parseExpression, expected, false);
}


test "correct operator associativity a / b / c" {
    const source =
        \\a / b / c
    ;
    
    const expected = .{
        AstNodeTag.BINARY_OP, Token.Tag.DIV, "/",
        .{ AstNodeTag.BINARY_OP, Token.Tag.DIV, "/", 
            .{ AstNodeTag.ATOM, "a" }, 
            .{ AstNodeTag.ATOM, "b" },
         },
        .{ AstNodeTag.ATOM, "c" },
    };

    try testParser(source, Parser.parseExpression, expected, false);
}

test "correct operator associativity a / b * c" {
    const source =
        \\a / b * c
    ;
    
    const expected = .{
        AstNodeTag.BINARY_OP, Token.Tag.TIMES, "*",
        .{ AstNodeTag.BINARY_OP, Token.Tag.DIV, "/", 
            .{ AstNodeTag.ATOM, "a" }, 
            .{ AstNodeTag.ATOM, "b" },
         },
        .{ AstNodeTag.ATOM, "c" },
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
        .{ AstNodeTag.DECLARATION, Token.Tag.IDENTIFIER, "x", .{ AstNodeTag.UNARY_OP, Token.Tag.MINUS, "-", .{ AstNodeTag.ATOM, "5.0" } } }, // x := -5.0
        .{ AstNodeTag.ASSIGNMENT, Token.Tag.ASSIGN, "=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "2.0" } }, // x = 2.0
        .{ // y := -x + -3 * - (-7 + -2) **-x
            AstNodeTag.DECLARATION, "y", //
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
            AstNodeTag.DECLARATION, "z", //
            .{
                AstNodeTag.BINARY_OP,      Token.Tag.POW,             "**", //
                .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "y" },
            },
        }, // z := x ** y
        .{ AstNodeTag.DECLARATION, "p", .{ AstNodeTag.ATOM, "7.1" } }, // p := 7.1
        .{
            AstNodeTag.DECLARATION, "result", //
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

    try testParser(source, Parser.parseIndentedCodeBlock, expected, false);
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
    const expected = .{ AstNodeTag.CALL_OR_INST, "doStuff" };
    try testParser(source, Parser.parseCallOrInst, expected, false);
}

test "parse function call wit 1 param" {
    const source = "doStuff(a)";
    const expected = .{ AstNodeTag.CALL_OR_INST, "doStuff", .{ AstNodeTag.ATOM, "a" } };
    try testParser(source, Parser.parseCallOrInst, expected, false);
}

test "parse function call wit 1 param + trailing comma" {
    const source = "doStuff(a,)";
    const expected = .{ AstNodeTag.CALL_OR_INST, "doStuff", .{"a"} };
    try testParser(source, Parser.parseCallOrInst, expected, false);
}

test "parse function call wit 2 params" {
    const source = "doStuff(a, b)";
    const expected = .{ AstNodeTag.CALL_OR_INST, "doStuff", .{"a"}, .{"b"} };
    try testParser(source, Parser.parseCallOrInst, expected, false);
}

test "parse function call wit 2 params + trailing comma" {
    const source = "doStuff(a, b,)";
    const expected = .{ AstNodeTag.CALL_OR_INST, "doStuff", .{"a"}, .{"b"} };
    try testParser(source, Parser.parseCallOrInst, expected, false);
}

test "parse complex program" {
    const source =
        \\fn main()
        \\    jahr := 0
        \\    zins := 1.02
        \\    result := 1.0
        \\    while jahr < 10
        \\         result *= zins
        \\         jahr += 1
        \\    
        \\    print(result)
    ;
    const expected = .{
        AstNodeTag.BLOCK, .{
            AstNodeTag.FNDECL, "main", .{AstNodeTag.FNPARAMS}, .{
                AstNodeTag.BLOCK,
                .{ AstNodeTag.DECLARATION, "jahr", .{ AstNodeTag.ATOM, "0" } },
                .{ AstNodeTag.DECLARATION, "zins", .{ AstNodeTag.ATOM, "1.02" } },
                .{ AstNodeTag.DECLARATION, "result", .{ AstNodeTag.ATOM, "1.0" } },
                .{
                    AstNodeTag.WHILE,
                    .{ AstNodeTag.BINARY_OP, "<", .{ AstNodeTag.ATOM, "jahr" }, .{ AstNodeTag.ATOM, "10" } },
                    .{
                        AstNodeTag.BLOCK,
                        .{ AstNodeTag.ASSIGNMENT, "*=", .{ AstNodeTag.ATOM, "result" }, .{ AstNodeTag.ATOM, "zins" } },
                        .{ AstNodeTag.ASSIGNMENT, "+=", .{ AstNodeTag.ATOM, "jahr" }, .{ AstNodeTag.ATOM, "1" } },
                    },
                },
                .{ AstNodeTag.CALL_OR_INST, "print", .{ AstNodeTag.ATOM, "result" } },
            },
        },
    };
    try testParser(source, Parser.parse, expected, false);
}

test "AST vs reference" {
    const src = "x := y + z";
    // parse src, get the AST, and compare it to the expected structure:
    const expected = .{
        AstNodeTag.DECLARATION,
        "x",
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

test "parse struct declaration" {
    const source =
        \\struct Car
        \\    velocity := 0.0
        \\    gear : Int
        \\    some_val
        \\    name : Int = 42
    ;
    const expected = .{
        AstNodeTag.STRUCTDECL, "Car",
        .{ AstNodeTag.DECLARATION, "velocity", .{ AstNodeTag.ATOM, "0.0" } },
        .{ AstNodeTag.DECLARATION, "gear", .{ AstNodeTag.TYPE, "Int" } },
        .{ AstNodeTag.STRUCT_MEMBER_BARE, "some_val" },
        .{ AstNodeTag.DECLARATION, "name", .{ AstNodeTag.TYPE, "Int" }, .{ AstNodeTag.ATOM, "42" } },
    };
    try testParser(source, Parser.parseStructDecl, expected, false);
}

test "parse named args" {
    const source = "Car(gear=3, name=42)";
    const expected = .{
        AstNodeTag.CALL_OR_INST, "Car",
        .{ AstNodeTag.NAMED_ARG, "gear", .{ AstNodeTag.ATOM, "3" } },
        .{ AstNodeTag.NAMED_ARG, "name", .{ AstNodeTag.ATOM, "42" } },
    };
    try testParser(source, Parser.parseCallOrInst, expected, false);
}

test "parse mixed positional and named args" {
    const source = "foo(1, 2, name=42)";
    const expected = .{
        AstNodeTag.CALL_OR_INST, "foo",
        .{ AstNodeTag.ATOM, "1" },
        .{ AstNodeTag.ATOM, "2" },
        .{ AstNodeTag.NAMED_ARG, "name", .{ AstNodeTag.ATOM, "42" } },
    };
    try testParser(source, Parser.parseCallOrInst, expected, false);
}

test "parse member access read" {
    const source = "s.velocity + 1";
    const expected = .{
        AstNodeTag.BINARY_OP, "+",
        .{ AstNodeTag.MEMBER_ACCESS, "velocity", .{ AstNodeTag.ATOM, "s" } },
        .{ AstNodeTag.ATOM, "1" },
    };
    try testParser(source, Parser.parseExpression, expected, false);
}

test "parse chained member access" {
    const source = "a.b.c";
    const expected = .{
        AstNodeTag.MEMBER_ACCESS, "c",
        .{ AstNodeTag.MEMBER_ACCESS, "b", .{ AstNodeTag.ATOM, "a" } },
    };
    try testParser(source, Parser.parseExpression, expected, false);
}

test "parse member assignment" {
    const source = "s.gear = 4\n";
    const expected = .{
        AstNodeTag.ASSIGNMENT, "=",
        .{ AstNodeTag.MEMBER_ACCESS, "gear", .{ AstNodeTag.ATOM, "s" } },
        .{ AstNodeTag.ATOM, "4" },
    };
    try testParser(source, Parser.parserDeclOrAssign, expected, false);
}

test "parse member compound assignment" {
    const source = "s.velocity += 0.5\n";
    const expected = .{
        AstNodeTag.ASSIGNMENT, "+=",
        .{ AstNodeTag.MEMBER_ACCESS, "velocity", .{ AstNodeTag.ATOM, "s" } },
        .{ AstNodeTag.ATOM, "0.5" },
    };
    try testParser(source, Parser.parserDeclOrAssign, expected, false);
}

test "parse typed declaration" {
    const source = "x : Int = 3\n";
    const expected = .{
        AstNodeTag.DECLARATION, "x",
        .{ AstNodeTag.TYPE, "Int" },
        .{ AstNodeTag.ATOM, "3" },
    };
    try testParser(source, Parser.parserDeclOrAssign, expected, false);
}

test "parse declaration without type" {
    const source = "x := 3\n";
    const expected = .{
        AstNodeTag.DECLARATION, "x",
        .{ AstNodeTag.ATOM, "3" },
    };
    try testParser(source, Parser.parserDeclOrAssign, expected, false);
}

test "parse struct in program" {
    const source =
        \\struct Point
        \\    x : Float
        \\    y : Float
        \\
        \\fn main()
        \\    p := Point(x=1.0, y=2.0)
        \\    p.x += 0.5
    ;
    const expected = .{
        AstNodeTag.BLOCK,
        .{
            AstNodeTag.STRUCTDECL, "Point",
            .{ AstNodeTag.DECLARATION, "x", .{ AstNodeTag.TYPE, "Float" } },
            .{ AstNodeTag.DECLARATION, "y", .{ AstNodeTag.TYPE, "Float" } },
        },
        .{
            AstNodeTag.FNDECL, "main", .{AstNodeTag.FNPARAMS}, .{
                AstNodeTag.BLOCK,
                .{
                    AstNodeTag.DECLARATION, "p",
                    .{
                        AstNodeTag.CALL_OR_INST, "Point",
                        .{ AstNodeTag.NAMED_ARG, "x", .{ AstNodeTag.ATOM, "1.0" } },
                        .{ AstNodeTag.NAMED_ARG, "y", .{ AstNodeTag.ATOM, "2.0" } },
                    },
                },
                .{
                    AstNodeTag.ASSIGNMENT, "+=",
                    .{ AstNodeTag.MEMBER_ACCESS, "x", .{ AstNodeTag.ATOM, "p" } },
                    .{ AstNodeTag.ATOM, "0.5" },
                },
            },
        },
    };
    try testParser(source, Parser.parse, expected, false);
}

test "parse struct as function param" {
    const source =
        \\struct Point
        \\    x : Float = 0.0
        \\    y : Float = 0.0
        \\
        \\fn length(p)
        \\    result = p.x * p.x + p.y * p.y
        \\
        \\fn main()
        \\    pt := Point(x=3.0, y=4.0)
        \\    result = length(pt)
    ;
    const expected = .{
        AstNodeTag.BLOCK,
        .{
            AstNodeTag.STRUCTDECL, "Point",
            .{ AstNodeTag.DECLARATION, "x", .{ AstNodeTag.TYPE, "Float" }, .{ AstNodeTag.ATOM, "0.0" } },
            .{ AstNodeTag.DECLARATION, "y", .{ AstNodeTag.TYPE, "Float" }, .{ AstNodeTag.ATOM, "0.0" } },
        },
        .{
            AstNodeTag.FNDECL, "length", .{
                .{ AstNodeTag.ATOM, "p" },
            }, .{
                AstNodeTag.BLOCK,
                .{
                    AstNodeTag.ASSIGNMENT, "=",
                    .{ AstNodeTag.ATOM, "result" },
                    .{
                        AstNodeTag.BINARY_OP, "+",
                        .{
                            AstNodeTag.BINARY_OP, "*",
                            .{ AstNodeTag.MEMBER_ACCESS, "x", .{ AstNodeTag.ATOM, "p" } },
                            .{ AstNodeTag.MEMBER_ACCESS, "x", .{ AstNodeTag.ATOM, "p" } },
                        },
                        .{
                            AstNodeTag.BINARY_OP, "*",
                            .{ AstNodeTag.MEMBER_ACCESS, "y", .{ AstNodeTag.ATOM, "p" } },
                            .{ AstNodeTag.MEMBER_ACCESS, "y", .{ AstNodeTag.ATOM, "p" } },
                        },
                    },
                },
            },
        },
        .{
            AstNodeTag.FNDECL, "main", .{AstNodeTag.FNPARAMS}, .{
                AstNodeTag.BLOCK,
                .{
                    AstNodeTag.DECLARATION, "pt",
                    .{
                        AstNodeTag.CALL_OR_INST, "Point",
                        .{ AstNodeTag.NAMED_ARG, "x", .{ AstNodeTag.ATOM, "3.0" } },
                        .{ AstNodeTag.NAMED_ARG, "y", .{ AstNodeTag.ATOM, "4.0" } },
                    },
                },
                .{
                    AstNodeTag.ASSIGNMENT, "=",
                    .{ AstNodeTag.ATOM, "result" },
                    .{
                        AstNodeTag.CALL_OR_INST, "length",
                        .{ AstNodeTag.ATOM, "pt" },
                    },
                },
            },
        },
    };
    try testParser(source, Parser.parse, expected, false);
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
