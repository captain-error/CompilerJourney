const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const sem = @import("semantic_analyzer.zig");

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

const AstNode = par.AstNode;
const Parser = par.Parser;
const AstNodeIndex = par.AstNodeIndex;

const SemanticValidator = sem.SemanticValidator;

const CPrecedenceLevel = i8;

pub const CGenerator = struct {
    ts: TokenStream,
    ast_nodes: []const AstNode,
    writer: *std.Io.Writer,

    // pub fn init(writer: *std.Io.Writer, ast_nodes: []const AstNode) CGenerator {
    //     return .{ .ast_nodes = ast_nodes, .writer=writer };
    // }

    fn precedenceOf(g: *const CGenerator, token_index: TokenIndex) CPrecedenceLevel {
        return switch (g.ts.tokens[token_index].tag) {
            .PLUS, .MINUS => 12,
            .STAR, .SLASH => 13,
            .STARSTAR => 14, // not technically a C operator
            else => 0,
        };
    }

    fn genHead(g: *CGenerator) !void {
        try g.writer.writeAll(
            \\#include <math.h>
            \\#include <stdio.h>
            \\
            \\int main(void) {
            \\
        );
    }

    fn genFoot(g: *CGenerator) !void {
        try g.writer.writeAll(
            \\  printf("result = %f\n", result);
            \\}
            \\
        );
    }

    fn genVariable(g: *CGenerator, token_idx: TokenIndex) !void {
        const token = g.ts.tokens[token_idx];
        std.debug.assert(token.tag == .IDENTIFIER);

        try g.writer.writeAll(g.ts.sourceStr(token_idx));
    }

    fn genAtom(g: *CGenerator, node: AstNode) !void {
        const token = g.ts.tokens[node.token_index];
        switch (token.tag) {
            .IDENTIFIER, .NUM => try g.writer.writeAll(g.ts.sourceStr(node.token_index)),
            else => unreachable,
        }
    }

    fn genBinaryOp(g: *CGenerator, node: AstNode, parent_precedence: CPrecedenceLevel) !void {
        const op_token = g.ts.tokens[node.token_index];
        if (op_token.tag == .STARSTAR) {
            try g.writer.writeAll("pow(");
            try g.genExpression(node.lhs);
            try g.writer.writeAll(", ");
            try g.genExpression(node.rhs);
            try g.writer.writeByte(')');
            return;
        }

        const precedence = g.precedenceOf(node.token_index);
        if (precedence < parent_precedence)
            try g.writer.writeByte('(');

        try g.genExpression1(node.lhs, precedence);
        try g.writer.writeByte(' ');
        try g.writer.writeAll(g.ts.sourceStr(node.token_index));
        try g.writer.writeByte(' ');
        try g.genExpression1(node.rhs, precedence);

        if (precedence < parent_precedence)
            try g.writer.writeByte(')');
    }

    fn genUnaryOp(g: *CGenerator, node: AstNode) !void {
        const child_idx = node.lhs;
        const child_node = g.ast_nodes[child_idx];

        const token = g.ts.tokens[node.token_index];
        switch (token.tag) {
            .MINUS => try g.writer.writeByte('-'),
            else => unreachable,
        }

        if (child_node.tag == .BINARY_OP)
            try g.writer.writeByte('(');

        try g.genExpression(child_idx);

        if (child_node.tag == .BINARY_OP)
            try g.writer.writeByte(')');
    }

    fn genExpression1(g: *CGenerator, idx: AstNodeIndex, parent_precedence: CPrecedenceLevel) std.Io.Writer.Error!void {
        const node = g.ast_nodes[idx];
        switch (node.tag) {
            .ATOM => try g.genAtom(node),
            .UNARY_OP => try g.genUnaryOp(node),
            .BINARY_OP => try g.genBinaryOp(node, parent_precedence),
            else => unreachable,
        }
    }

    fn genExpression(g: *CGenerator, idx: AstNodeIndex) std.Io.Writer.Error!void {
        return try g.genExpression1(idx, 0);
    }

    fn genDecl(g: *CGenerator, lhs: AstNodeIndex, rhs: AstNodeIndex) !void {
        try g.writer.writeAll("  double ");
        const lhs_node = g.ast_nodes[lhs];
        std.debug.assert(lhs_node.tag == .ATOM);
        try g.genVariable(lhs_node.token_index);
        try g.writer.writeAll(" = ");
        try g.genExpression(rhs);
        try g.writer.writeAll(";\n");
    }

    fn genAssignment(g: *CGenerator, node: AstNode) !void {
        try g.writer.writeAll("  ");
        const lhs_node = g.ast_nodes[node.lhs];
        std.debug.assert(lhs_node.tag == .ATOM);
        try g.genVariable(lhs_node.token_index);

        try g.writer.writeByte(' ');
        try g.writer.writeAll(g.ts.sourceStr(node.token_index));
        try g.writer.writeByte(' ');

        try g.genExpression(node.rhs);
        try g.writer.writeAll(";\n");
    }

    fn genCommand(g: *CGenerator, command_idx: AstNodeIndex) !void {
        const node = g.ast_nodes[command_idx];
        std.debug.assert(node.tag == .COMMAND);

        const op = g.ts.tokens[node.token_index];
        switch (op.tag) {
            .COLONEQ => try g.genDecl(node.lhs, node.rhs),
            .EQ, .MINUSEQ, .PLUSEQ, .STAREQ, .SLASHEQ => try g.genAssignment(node),
            else => unreachable,
        }
    }

    pub fn generate(g: *CGenerator, top_level_ast_indices: []const AstNodeIndex) !void {
        try g.genHead();

        for (top_level_ast_indices) |command_idx| {
            try g.genCommand(command_idx);
        }

        try g.genFoot();
        try g.writer.flush();
    }
};
