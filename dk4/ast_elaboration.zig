const std = @import("std");
const par = @import("parser.zig");
const tok = @import("tokenizer.zig");
const test_util = @import("test_util.zig");

const AstNodeIndex = par.AstNodeIndex;
const AstNodeTag = par.AstNodeTag;
const AST = par.AST;
const Token = tok.Token;
const TokenStream = tok.TokenStream;

pub const Error = union(enum) {
    illegal_assignment_target: struct {
        assignment_idx: AstNodeIndex,
    },
    assignment_in_expression: AstNodeIndex,


    pub fn print(err : Error, ast : *const AST, ts : TokenStream, writer : *std.Io.Writer) !void {
        switch (err) {
            .illegal_assignment_target => |e| {
                const pos = try par.printLineAndMarkAstNode(ast, writer, ts, e.assignment_idx, null);
                const assignement_node = ast.get(e.assignment_idx);
                const lhs_node = ast.get(assignement_node.first_child);
                // par.printAstBranch(writer, e.assignment_idx, ast, ts.tokens, ts.source, 0);
                const token = ts.tokens[lhs_node.token_index];
                try writer.print("line {}: Error: Illegal assignment target: {s}\n", .{pos.line, token.str(ts.source)});
            },
            .assignment_in_expression => |idx| {
                const pos = try par.printLineAndMarkAstNode(ast, writer, ts, idx, null);
                const assignement_node = ast.get(idx);
                const token = ts.tokens[assignement_node.token_index];
                try writer.print("line {}: Error: Assignment used in expression: {s}\n", .{pos.line, token.str(ts.source)});
            },
        }  
    } 
};

const Elaborator = struct {
    ast: *AST,
    tokens: []const Token,
    gpa: std.mem.Allocator,
    errors: std.ArrayList(Error),

    fn elaborateNode(e: *Elaborator, idx: AstNodeIndex, assignment_allowed: bool) !void {
        if (idx == 0) return;
        const node = e.ast.get(idx);

        if (node.tag == .BINARY_OP and isAssignOp(e.tokens[node.token_index].tag)) {
            if (assignment_allowed) {
                node.tag = .ASSIGNMENT;
                if (!isValidLValue(e.ast, node.first_child))
                    try e.errors.append(e.gpa, .{ .illegal_assignment_target = .{
                        .assignment_idx = idx,
                    } });
            } else {
                try e.errors.append(e.gpa, .{ .assignment_in_expression = idx });
            }
        }

        const child_allowed = node.tag == .BLOCK or node.tag == .DEFER;
        var child_idx = node.first_child;
        while (child_idx != 0) : (child_idx = e.ast.get(child_idx).next_sibling){
            try e.elaborateNode(child_idx, child_allowed); 
        }
    }
};

pub fn elaborate(
    ast: *AST,
    tokens: []const Token,
    root_idx: AstNodeIndex,
    gpa: std.mem.Allocator,
) !std.ArrayList(Error) {
    var e = Elaborator{
        .ast = ast,
        .tokens = tokens,
        .gpa = gpa,
        .errors = .empty,
    };
    try e.elaborateNode(root_idx, false);
    return e.errors;
}

fn isValidLValue(ast: *const AST, idx: AstNodeIndex) bool {
    if (idx == 0) return false;
    return switch (ast.get(idx).tag) {
        .ATOM, .MEMBER_ACCESS, .ARRAY_ACCESS => true,
        else => false,
    };
}

fn isAssignOp(tag: Token.Tag) bool {
    return switch (tag) {
        .ASSIGN, .PLUSASSIGN, .MINUSASSIGN, .MULTASSIGN, .DIVASSIGN => true,
        else => false,
    };
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
    try parseAndElab(source, expected, false);
}
test "parse program with defers" {
        const source =
        \\fn main()
        \\    x := 3.0
        \\    if x < 10.0
        \\        x -= 0.1
        \\        defer x += 0.2
        \\        x += 1.0
        \\    defer a := 2
        \\    defer b := 3
        \\    result = x + 1.0;
    ;

    const expected = .{
        AstNodeTag.BLOCK, .{
            AstNodeTag.FNDECL, "main", .{AstNodeTag.FNPARAMS}, .{
                AstNodeTag.BLOCK,
                .{ AstNodeTag.DECLARATION, "x", .{ AstNodeTag.ATOM, "3.0" } },
                .{ AstNodeTag.IF,
                    .{ AstNodeTag.BINARY_OP, "<", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "10.0" } },
                    .{ AstNodeTag.BLOCK,
                        .{ AstNodeTag.ASSIGNMENT, "-=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "0.1" } },
                        .{ AstNodeTag.DEFER, .{ AstNodeTag.ASSIGNMENT, "+=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "0.2" } } },
                        .{ AstNodeTag.ASSIGNMENT, "+=", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "1.0" } },
                    },
                },
                .{ AstNodeTag.DEFER, .{ AstNodeTag.DECLARATION, "a", .{ AstNodeTag.ATOM, "2" } } },
                .{ AstNodeTag.DEFER, .{ AstNodeTag.DECLARATION, "b", .{ AstNodeTag.ATOM, "3" } } },
                .{ AstNodeTag.ASSIGNMENT, "=", .{ AstNodeTag.ATOM, "result" }, .{ AstNodeTag.BINARY_OP, "+", .{ AstNodeTag.ATOM, "x" }, .{ AstNodeTag.ATOM, "1.0" } } },
            },
        },
    };
    try parseAndElab(source, expected, false);
}


fn parseAndElab(source: []const u8, expected_structure: anytype, print_always: bool) !void {
    const gpa = std.testing.allocator;
    var threaded: std.Io.Threaded = .init(gpa, .{});
    defer threaded.deinit();
    const io = threaded.io();

    var stdout_buff: [1024]u8 = undefined;
    var stdout_file = std.Io.File.stdout();
    var stdout_writer = stdout_file.writer(io, &stdout_buff);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch unreachable;
    // const terminal = std.Io.Terminal{
    //     .writer = stdout,
    //     .mode = try std.Io.Terminal.Mode.detect(io, stdout_file, false, false),
    // };

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var parser = try par.Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();
    const ast_idx = parser.parse() catch |err| {
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
    
    
    var elab_errors = try elaborate(&parser.ast, ts.tokens, parser.root_node, gpa);
    defer elab_errors.deinit(gpa);

     if (elab_errors.items.len > 0) {
        try stdout.print("ELABORATION REPORTED ERRORS:\n", .{});
        for (elab_errors.items) |err| {
            try err.print(&parser.ast, ts, stdout);
        }
        return error.ElaborationReportedErrors;
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
        test_util.testAstStructure(&parser.ast);
        return error.NotAllTokensUsed;
    }

    test_util.expectAstStructure(&parser.ast, &ts, stdout, ast_idx, 0, expected_structure) catch |err| {
        try stdout.writeAll("Writing complete AST because of non-matching structure:\n");
        try parser.printAstBranch(stdout, ast_idx, 1);

        return err;
    };

    try stdout.flush();
}