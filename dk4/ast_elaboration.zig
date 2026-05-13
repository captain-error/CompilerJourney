const std = @import("std");
const par = @import("parser.zig");
const tok = @import("tokenizer.zig");

const AstNodeIndex = par.AstNodeIndex;
const AstNodeTag = par.AstNodeTag;
const AST = par.AST;
const Token = tok.Token;

pub const Error = union(enum) {
    illegal_assignment_target: struct {
        assignment_idx: AstNodeIndex,
    },
    assignment_in_expression: AstNodeIndex,
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

        const child_allowed = (node.tag == .BLOCK);
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
