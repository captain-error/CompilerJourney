const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const typing = @import("type_inference.zig");
const assert = std.debug.assert;


const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

const AstNode = par.AstNode;
const AST = par.AST;
const Parser = par.Parser;
const AstNodeIndex = par.AstNodeIndex;

const DkType = typing.DkType;

const TypeInferer = typing.TypeInferer;

const CPrecedenceLevel = i8;

pub const CGenerator = struct {
    ts: TokenStream,
    ast: AST, // not owned!
    node_types: []DkType,
    writer: *std.Io.Writer,
    indent_lvl: u16 = 0,

    // pub fn init(writer: *std.Io.Writer, ast_nodes: []const AstNode) CGenerator {
    //     return .{ .ast_nodes = ast_nodes, .writer=writer };
    // }

    pub const CGeneratorException = error{WriteFailed};

    /// C's precedence for for binary operators.
    fn precedenceOf(tag: Token.Tag) CPrecedenceLevel {
        return switch (tag) {
            // , => 1
            .ASSIGN, .DECLARE, .PLUSASSIGN, .MINUSASSIGN, .MULTASSIGN, .DIVASSIGN => 2,
            // ?: => 3
            .OR => 4,
            // .XOR => 4, // THERE IS NO LOGICAL XOR IN C!
            .AND => 5,
            // | ==> 6
            .BW_XOR => 7,
            // & ==> 8,
            .EQ, .NOT_EQ => 9,
            .GT, .GE, .LT, .LE => 10,
            // << >> => 11,
            .PLUS, .MINUS => 12,
            .TIMES, .DIV => 13,
            .POW => 14, // not technically a C operator
            else => unreachable,
        };
    }

    fn cOperatorSymbol(tag: Token.Tag) []const u8 {
        return switch (tag) {
            // , => 0
            .DECLARE, .ASSIGN => "=",
            .PLUSASSIGN => "+=",
            .MINUSASSIGN => "-=",
            .MULTASSIGN => "*=",
            .DIVASSIGN => "/=",
            // ?: => 2
            .OR => "||",
            .XOR => unreachable, // THERE IS NO LOGICAL XOR IN C!
            .AND => "&&",
            // | ==> 6
            // ^ => 7
            // & ==> 8,
            .EQ => "==",
            .NOT_EQ => "!=",
            .GT => ">",
            .GE => ">=",
            .LT => "<",
            .LE => "<=",
            // << >> => 11,
            .PLUS => "+",
            .MINUS => "-",
            .TIMES => "*",
            .DIV => "/",
            .POW => unreachable, // not a C operator
            else => unreachable,
        };
    }

    fn genHead(g: *const CGenerator) !void {
        try g.writer.writeAll(
            \\#include <stdint.h>
            \\#include <math.h>
            \\#include <stdio.h>
            \\
            \\void print(double val) {
            \\    printf("%f\n", val);
            \\}
            \\
            \\int main(void) {
            \\
        );
    }

    fn genFoot(g: *const CGenerator) !void {
        try g.writer.writeAll(
            \\    return 0;
            \\}
            \\
        );
    }

    fn genIndentation(g: *const CGenerator) !void {
        try g.writer.splatByteAll(' ', 4 * g.indent_lvl);
    }

    // fn genVariable(g: *CGenerator, token_idx: TokenIndex) !void {
    //     const token = g.ts.tokens[token_idx];
    //     std.debug.assert(token.tag == .IDENTIFIER);

    //     try g.writer.writeAll(g.ts.sourceStr(token_idx));
    // }

    fn genAtom(g: *CGenerator, node: *const AstNode) !void {
        const token = g.ts.tokens[node.token_index];
        switch (token.tag) {
            .IDENTIFIER, .FLOAT_LIT, .INT_LIT => try g.writer.writeAll(g.ts.sourceStr(node.token_index)),
            else => unreachable,
        }
    }

    fn genBinaryOp(g: *CGenerator, node: *const AstNode, parent_precedence: CPrecedenceLevel) !void {
        const op_token = g.ts.tokens[node.token_index];
        const lhs_idx = node.first_child;
        const lhs_node = g.ast.get(lhs_idx);
        const rhs_idx = lhs_node.next_sibling;

        op_switch: switch (op_token.tag) {
            .POW => {
                try g.writer.writeAll("pow(");
                try g.genExpression(lhs_idx);
                try g.writer.writeAll(", ");
                try g.genExpression(rhs_idx);
                try g.writer.writeByte(')');
            },
            // there is no logical xor in C, so we use bitwise xor.
            // The typesystem guarantees that the operants are booleans
            // and the precedenec level handling guarantees correct parentesis
            .XOR => continue :op_switch .BW_XOR,
            else => {
                const precedence = precedenceOf(op_token.tag);

                if (precedence < parent_precedence)
                    try g.writer.writeByte('(');

                try g.genExpression1(lhs_idx, precedence);
                try g.writer.writeByte(' ');
                try g.writer.writeAll(cOperatorSymbol(op_token.tag));
                try g.writer.writeByte(' ');
                try g.genExpression1(rhs_idx, precedence);

                if (precedence < parent_precedence)
                    try g.writer.writeByte(')');
            },
        }
    }

    fn genUnaryOp(g: *CGenerator, node: *const AstNode) !void {
        const child_idx = node.first_child;

        const token = g.ts.tokens[node.token_index];
        switch (token.tag) {
            .MINUS => try g.writer.writeByte('-'),
            .NOT => try g.writer.writeByte('!'),
            .BW_NOT => try g.writer.writeByte('~'),
            else => unreachable,
        }

        try g.genExpression1(child_idx, 99); // we set a really high precedenec to force the child to be parenthesized if it is a binary op.
    }

    fn genExpression1(g: *CGenerator, node_idx: AstNodeIndex, parent_precedence: CPrecedenceLevel) std.Io.Writer.Error!void {
        const node = g.ast.get(node_idx);
        switch (node.tag) {
            .ATOM => try g.genAtom(node),
            .UNARY_OP => try g.genUnaryOp(node),
            .FNCALL => try g.genFunCallExpression(node),
            .BINARY_OP => try g.genBinaryOp(node, parent_precedence),
            else => unreachable,
        }
    }

    fn genExpression(g: *CGenerator, node_idx: AstNodeIndex) std.Io.Writer.Error!void {
        return try g.genExpression1(node_idx, 0);
    }

    fn genFunCallExpression(g: *CGenerator, node: *const AstNode) !void {

        const token = g.ts.tokens[node.token_index];
        const fname = token.str(g.ts.source);
        try g.writer.writeAll(fname);
        try g.writer.writeByte('(');

        var child_list = node.children(&g.ast);
        var first_arg = true;
        while (child_list.nextIdx()) |child_idx| {
            if (!first_arg)
                try g.writer.writeByte(',');
            first_arg = false;
            try g.genExpression(child_idx);
        }
        try g.writer.writeByte(')');
    }

    fn genFunCallStatement(g: *CGenerator, node_idx: AstNodeIndex) !void {
        const node = g.ast.get(node_idx);
        try g.genIndentation();
        try g.genFunCallExpression(node);
        try g.writer.writeAll(";\n");
    }

    fn genTypeName(g: *CGenerator, node_idx: AstNodeIndex) !void {
        const node_type = g.node_types[node_idx];

        const type_str = switch (node_type) {
            .BOOL => "bool",
            .INT => "int64_t",
            .FLOAT => "double",
            else => unreachable,
        };

        try g.writer.writeAll(type_str);
    }

    fn genDecl(g: *CGenerator, node_idx: AstNodeIndex) !void {
        const node = g.ast.get(node_idx);
        try g.genIndentation();

        try g.genTypeName(node_idx);
        try g.writer.writeByte(' ');

        const var_node = g.ast.get(node.first_child);
        try g.genAtom(var_node);

        try g.writer.writeAll(" = ");
        try g.genExpression(var_node.next_sibling);
        try g.writer.writeAll(";\n");
    }

    fn genAssignment(g: *CGenerator, node_idx: AstNodeIndex) !void {
        const node = g.ast.get(node_idx);
        try g.genIndentation();

        const var_node = g.ast.get(node.first_child);
        try g.genAtom(var_node);

        try g.writer.writeByte(' ');
        const op_token_idx = node.token_index;
        try g.writer.writeAll(g.ts.sourceStr(op_token_idx));
        try g.writer.writeByte(' ');

        try g.genExpression(var_node.next_sibling);
        try g.writer.writeAll(";\n");
    }

    fn genBlock(g: *CGenerator, node_idx: AstNodeIndex) CGeneratorException!void {
        const node = g.ast.get(node_idx);
        assert(node.tag == .BLOCK);
        try g.genIndentation();
        try g.writer.writeAll("{\n");
        g.indent_lvl += 1;

        var child_list = node.children(&g.ast);
        while (child_list.nextIdx()) |child_idx|
            try g.genStatement(child_idx);

        g.indent_lvl -= 1;
        try g.genIndentation();
        try g.writer.writeAll("}\n");
    }

    fn genWhile(g: *CGenerator, node_idx: AstNodeIndex) !void {
        const node = g.ast.get(node_idx);
        assert(node.tag == .WHILE);

        const cond_idx = node.first_child;
        assert(cond_idx > 0);

        try g.genIndentation();
        try g.writer.writeAll("while (");
        try g.genExpression(cond_idx);
        try g.writer.writeAll(")\n");

        // create body:
        const cond_node = g.ast.get(cond_idx);
        const body_idx = cond_node.next_sibling;
        assert(body_idx > 0);
        try g.genBlock(body_idx);
    }

    fn genIf(g: *CGenerator, node_idx: AstNodeIndex) !void {
        const node = g.ast.get(node_idx);
        assert(node.tag == .IF);

        const cond_idx = node.first_child;
        assert(cond_idx > 0);

        try g.genIndentation();
        try g.writer.writeAll("if (");
        try g.genExpression(cond_idx);
        try g.writer.writeAll(")\n");

        // create body:
        const cond_node = g.ast.get(cond_idx);
        const then_idx = cond_node.next_sibling;
        assert(then_idx > 0);
        try g.genBlock(then_idx);

        const then_node = g.ast.get(then_idx);
        const else_idx = then_node.next_sibling;
        if (else_idx > 0) {
            try g.genIndentation();
            try g.writer.writeAll("else\n");
            try g.genBlock(then_idx);
        }
    }

    fn genStatement(g: *CGenerator, node_idx: AstNodeIndex) !void {
        const node = g.ast.get(node_idx);
        switch (node.tag) {
            .DECLARATION => try g.genDecl(node_idx),
            .ASSIGNMENT => try g.genAssignment(node_idx),
            .WHILE => try g.genWhile(node_idx),
            .IF => try g.genIf(node_idx),
            .FNCALL => try g.genFunCallStatement(node_idx),
            else => {
                std.debug.print("INTERNAL ERROR: {} #{}\n", .{node.tag, node_idx});
                unreachable;
            },
        }
    }

    pub fn generate(g: *CGenerator, root_node: AstNodeIndex) !void {
        try g.genHead();

        try g.genBlock(root_node);

        try g.genFoot();
        try g.writer.flush();
    }
};
