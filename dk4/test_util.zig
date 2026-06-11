const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");

const assert = std.debug.assert;

const Token = tok.Token;
const TokenStream = tok.TokenStream;
// const TokenIndex = TokenStream.TokenIndex;
// const SourceLoc = tok.SourceLoc;

const AST = par.AST;
// const AstNode = par.AstNode;
// const Parser = par.Parser;
const AstNodeIndex = par.AstNodeIndex;
const AstNodeTag = par.AstNodeTag;

pub fn testAstStructure(ast: *const AST) void {
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
            .DEFER            => assert(num_kids == 1), // statement or block
            .CALL_OR_INST     => {},
            .NAMED_ARG        => assert(num_kids == 1),
            .FNPARAMS         => {},
            .BREAK            => assert(num_kids == 0),
            .CONTINUE         => assert(num_kids == 0),
            .RETURN           => assert(num_kids == 1),
            .FOR              => {}, // FOR_INIT? → FOR_COND? → FOR_INCR? → BLOCK
            .FOR_INIT         => assert(num_kids == 1), // one child: init expression/statement
            .FOR_COND         => assert(num_kids == 1), // one child: condition expression
            .FOR_INCR         => assert(num_kids == 1), // one child: increment expression
            .FNDECL           => assert(num_kids == 2 or num_kids == 3), // FNPARAMS + [optional TYPE] + BODY
            .STRUCTDECL       => {},
            .MEMBER           => {assert(num_kids >= 0); assert(num_kids <= 2);},
            .PARAM            => {assert(num_kids >= 0); assert(num_kids <= 2);},
            .TYPE             => assert(num_kids == 0 or num_kids == 1), // 0: scalar, 1: array (ARRAY_SHAPE child)
            .MEMBER_ACCESS    => assert(num_kids == 1),
            .ARRAY_LIT        => {},
            .FILL             => assert(num_kids == 1),
            .ARRAY_SHAPE      => assert(num_kids >= 1),
            .INFER_DIM        => assert(num_kids == 0),
            .ARRAY_ACCESS     => assert(num_kids == 2),
            .INDEX_ARGS       => assert(num_kids >= 1),
            // zig fmt: on
        }
    }
}

pub fn expectAstStructure(
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
