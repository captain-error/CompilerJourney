const std = @import("std");
const par = @import("parser.zig");
const tok = @import("tokenizer.zig");

const AstNodeIndex = par.AstNodeIndex;
const AstNodeTag = par.AstNodeTag;
const AST = par.AST;
const Token = tok.Token;
const TokenIndex = tok.TokenStream.TokenIndex;

pub const Error = union(enum) {
    illegal_assignment_target: struct {
        assignment_idx: AstNodeIndex,
    },
    assignment_in_expression: AstNodeIndex,
    break_continue_outside_loop: TokenIndex,
};

const Elaborator = struct {
    ast: *AST,
    tokens: []const Token,
    gpa: std.mem.Allocator,
    errors: std.ArrayList(Error),

    fn elaborateNode(e: *Elaborator, idx: AstNodeIndex, assignment_allowed: bool, loop_depth: u8) !void {
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

        // break/continue must be inside a loop
        if (node.tag == .BREAK or node.tag == .CONTINUE) {
            if (loop_depth == 0)
                try e.errors.append(e.gpa, .{ .break_continue_outside_loop = node.token_index });
            return; // leaf nodes, no children
        }

        // WHILE: body children are in a loop context
        if (node.tag == .WHILE) {
            try e.elaborateNode(node.first_child, false, loop_depth); // condition
            const body_idx = e.ast.get(node.first_child).next_sibling;
            if (body_idx != 0)
                try e.elaborateNode(body_idx, true, loop_depth + 1); // body
            return;
        }

        const child_allowed = (node.tag == .BLOCK);
        var child_idx = node.first_child;
        while (child_idx != 0) : (child_idx = e.ast.get(child_idx).next_sibling){
            try e.elaborateNode(child_idx, child_allowed, loop_depth);
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
    try e.elaborateNode(root_idx, false, 0);
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
