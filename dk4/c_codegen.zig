const std = @import("std");
const ft_ast = @import("ft_ast.zig");

const FtAst = ft_ast.FtAst;
const Scope = ft_ast.Scope;
const VarDecl = ft_ast.VarDecl;
const FnDecl = ft_ast.FnDecl;
const Expression = ft_ast.Expression;
const Statement = ft_ast.Statement;
const ScopeIndex = ft_ast.ScopeIndex;
const StamentIndex = ft_ast.StamentIndex;
const ExpressionIndex = ft_ast.ExpressionIndex;
const VarDeclIndex = ft_ast.VarDeclIndex;
const FnDeclIndex = ft_ast.FnDeclIndex;

const DkType = ft_ast.DkType;

const BinaryOp = ft_ast.BinaryOp;
const AssignmentKind = ft_ast.AssignmentKind;

const Writer = std.Io.Writer;

const loop_var_names = [ft_ast.MAX_ARRAY_NDIM][]const u8{
    "_dk_i0", "_dk_i1", "_dk_i2", "_dk_i3",
    "_dk_i4", "_dk_i5", "_dk_i6", "_dk_i7",
};

const IndexVal = union(enum) {
    literal: u16,
    loop_var: u8, // dim number → loop_var_names[dim]
};

const CCodegen = struct {
    ft: *const FtAst,
    w: *Writer,
    indent: u32,

    const EmitError = Writer.Error;

    pub fn generate(w: *Writer, ft: *const FtAst) EmitError!void {
        var self = CCodegen{
            .ft = ft,
            .w = w,
            .indent = 0,
        };

        try self.emitIncludes();
        try self.emitByte('\n');
        try self.emitStructTypedefs();
        try self.emitForwardDeclarations();
        try self.emitByte('\n');
        try self.emitAllFunctions();
    }

    // ----- Includes -----

    fn emitIncludes(self: *CCodegen) EmitError!void {
        try self.emitStr("#include <stdint.h>\n");
        try self.emitStr("#include <stdbool.h>\n");
        try self.emitStr("#include <math.h>\n");
        try self.emitStr("#include <stdio.h>\n");
    }

    // ----- Struct typedefs -----

    fn emitStructTypedefs(self: *CCodegen) EmitError!void {
        for (self.ft.struct_decls.items) |sd| {
            try self.emitStr("typedef struct {\n");
            const members = self.ft.var_decls.items[sd.members.start..sd.members.end];
            for (members) |m| {
                try self.emitStr("    ");
                try self.emitTypeName(m.type_);
                try self.emitByte(' ');
                try self.emitStr(m.name);
                try self.emitStr(";\n");
            }
            try self.emitStr("} ");
            try self.emitMangledStructName(sd);
            try self.emitStr(";\n\n");
        }
    }

    // ----- Forward declarations -----

    fn emitForwardDeclarations(self: *CCodegen) EmitError!void {
        const decls = self.ft.fn_decls.items[1..];
        var i: usize = decls.len;
        while (i > 0) {
            i -= 1;
            const fn_decl = decls[i];
            if(!std.mem.eql(u8, fn_decl.name, "main")) {
                try self.emitFnSignature(fn_decl);
                try self.emitStr(";\n");
            }
        }
    }

    // ----- Functions -----

    fn emitAllFunctions(self: *CCodegen) EmitError!void {
        const decls = self.ft.fn_decls.items[1..];
        var i: usize = decls.len;
        while (i > 0) {
            i -= 1;
            if (i < decls.len - 1)
                try self.emitByte('\n');
            try self.emitFunction(decls[i]);
        }
    }

    fn emitFunction(self: *CCodegen, fn_decl: FnDecl) EmitError!void {
        const is_main = std.mem.eql(u8, fn_decl.name, "main");

        try self.emitFnSignature(fn_decl);
        try self.emitStr(" {\n");
        self.indent += 1;

        // Emit result variable declaration (non-void, non-main gets it too; main gets it for printing)
        if (fn_decl.return_type != DkType.VOID or is_main) {
            if (is_main) {
                // main uses double result for the printf
                // Actually, main's return_type should be whatever the program returns
                // but C main returns int. The result var holds the computed value.
            }
            if (fn_decl.return_type != DkType.VOID) {
                try self.emitIndent();
                try self.emitTypeName(fn_decl.return_type);
                try self.emitStr(" result;\n");
            }
        }

        // Emit body statements
        try self.emitScopeStatements(fn_decl.body_scope);

        // Main epilogue: printf + return 0
        if (is_main) {
            if (fn_decl.return_type != DkType.VOID) {
                try self.emitIndent();
                try self.emitPrintf(fn_decl.return_type);
                try self.emitByte('\n');
            }
            try self.emitIndent();
            try self.emitStr("return 0;\n");
        } else if (fn_decl.return_type != DkType.VOID) {
            try self.emitIndent();
            try self.emitStr("return result;\n");
        }

        self.indent -= 1;
        try self.emitStr("}\n");
    }

    fn emitFnSignature(self: *CCodegen, fn_decl: FnDecl) EmitError!void {
        const is_main = std.mem.eql(u8, fn_decl.name, "main");

        if (is_main) {
            try self.emitStr("int main(void)");
            return;
        }

        try self.emitTypeName(fn_decl.return_type);
        try self.emitByte(' ');
        try self.emitMangledFnName(fn_decl);
        try self.emitByte('(');

        const params = self.ft.var_decls.items[fn_decl.params.start..fn_decl.params.end];
        for (params, 0..) |param, i| {
            if (i > 0)
                try self.emitStr(", ");
            if (param.type_.isArray()) {
                try self.emitArrayParam(param.name, param.type_);
            } else {
                try self.emitTypeName(param.type_);
                try self.emitByte(' ');
                try self.emitStr(param.name);
            }
        }
        try self.emitByte(')');
    }

    fn emitMangledFnName(self: *CCodegen, fn_decl: FnDecl) EmitError!void {
        try self.emitStr(fn_decl.name);
        const params = self.ft.var_decls.items[fn_decl.params.start..fn_decl.params.end];
        for (params) |param| {
            try self.emitStr("__");
            try self.emitMangledTypeName(param.type_);
        }
    }

    fn emitMangledStructName(self: *CCodegen, sd: ft_ast.StructDecl) EmitError!void {
        try self.emitStr(sd.name);
        if (!sd.generic) return;
        const members = self.ft.var_decls.items[sd.members.start..sd.members.end];
        for (members) |m| {
            try self.emitStr("__");
            try self.emitMangledTypeName(m.type_);
        }
    }

    fn emitMangledTypeName(self: *CCodegen, t: DkType) EmitError!void {
        if (t.isStruct()) {
            try self.emitMangledStructName(self.ft.struct_decls.items[t.structInstanceIdx()]);
        } else if (t.isArray()) {
            const ai = &self.ft.array_instances.items[t.arrayInstanceIdx()];
            try self.emitStr("Arr");
            for (0..ai.ndim) |i| {
                try self.emitByte('_');
                try self.w.print("{d}", .{ai.shape[i]});
            }
            try self.emitByte('_');
            try self.emitStr(ai.elem_type.langName());
        } else {
            try self.emitStr(t.langName());
        }
    }

    // ----- Statements -----

    fn emitScopeStatements(self: *CCodegen, scope_idx: ScopeIndex) EmitError!void {
        const scope = self.ft.scopes.items[scope_idx];
        var stmt_idx = scope.first_statement;
        while (stmt_idx != 0) {
            const stmt = self.ft.statements.items[stmt_idx];
            try self.emitStatement(stmt);
            stmt_idx = stmt.next_sibling;
        }
    }

    fn emitStatement(self: *CCodegen, stmt: Statement) EmitError!void {
        switch (stmt.kind) {
            .VAR_DECL => |vd| {
                const var_decl = self.ft.var_decls.items[vd.var_decl_idx];
                if (var_decl.type_.isArray()) {
                    try self.emitArrayLitAssignment(var_decl.name, var_decl.type_, vd.rhs);
                } else {
                    try self.emitIndent();
                    try self.emitTypeName(var_decl.type_);
                    try self.emitByte(' ');
                    try self.emitStr(var_decl.name);
                    try self.emitStr(" = ");
                    try self.emitExpression(vd.rhs, 0, false);
                    try self.emitStr(";\n");
                }
            },
            .ASSIGNMENT => |a| {
                try self.emitIndent();
                try self.emitExpression(a.lhs, 0, false);
                try self.emitStr(switch (a.kind) {
                    .ASSIGN => " = ",
                    .PLUS => " += ",
                    .MINUS => " -= ",
                    .MULT => " *= ",
                    .DIV => " /= ",
                });
                try self.emitExpression(a.rhs, 0, false);
                try self.emitStr(";\n");
            },
            .IF_STMT => |ifs| {
                try self.emitIndent();
                try self.emitStr("if (");
                try self.emitExpression(ifs.condition, 0, false);
                try self.emitStr(") {\n");
                self.indent += 1;
                try self.emitScopeStatements(ifs.then_scope);
                self.indent -= 1;
                try self.emitIndent();
                if (ifs.else_scope != 0) {
                    try self.emitStr("} else {\n");
                    self.indent += 1;
                    try self.emitScopeStatements(ifs.else_scope);
                    self.indent -= 1;
                    try self.emitIndent();
                }
                try self.emitStr("}\n");
            },
            .WHILE_LOOP => |wl| {
                try self.emitIndent();
                try self.emitStr("while (");
                try self.emitExpression(wl.condition, 0, false);
                try self.emitStr(") {\n");
                self.indent += 1;
                try self.emitScopeStatements(wl.body_scope);
                self.indent -= 1;
                try self.emitIndent();
                try self.emitStr("}\n");
            },
            .FN_CALL => |expr_idx| {
                try self.emitIndent();
                try self.emitExpression(expr_idx, 0, false);
                try self.emitStr(";\n");
            },
            .INVALID => unreachable,
        }
    }

    // ----- Expressions -----

    fn emitExpression(self: *CCodegen, expr_idx: ExpressionIndex, parent_prec: u8, is_rhs:bool) EmitError!void {
        const expr = self.ft.expressions.items[expr_idx];
        switch (expr.kind) {
            .VAR_REF => |vd_idx| {
                const var_decl = self.ft.var_decls.items[vd_idx];
                try self.emitStr(var_decl.name);
            },
            .LITERAL => |lit| switch (lit) {
                .int => |v| try self.w.print("{d}", .{v}),
                .float => |v| try self.w.print("{e}", .{v}),
                .boolean => |v| try self.emitStr(if (v) "true" else "false"),
            },
            .BINARY_OP => |bin| {
                if (bin.op == .POW) {
                    // pow(lhs, rhs) — cast to int64_t if result is INT
                    if (expr.type_ == DkType.INT)
                        try self.emitStr("(int64_t)");
                    try self.emitStr("pow(");
                    try self.emitExpression(bin.lhs, 0, false);
                    try self.emitStr(", ");
                    try self.emitExpression(bin.rhs, 0, false);
                    try self.emitByte(')');
                } else {
                    const prec = opPrecedence(bin.op);
                    // _ = is_rhs;
                    const need_parens = prec < parent_prec or (prec == parent_prec and is_rhs);
                    if (need_parens)
                        try self.emitByte('(');
                    try self.emitExpression(bin.lhs, prec, false);
                    try self.emitStr(opStr(bin.op));
                    try self.emitExpression(bin.rhs, prec, true); // +1 for left-associativity
                    if (need_parens)
                        try self.emitByte(')');
                }
            },
            .UNARY_OP => |un| switch (un.op) {
                .NEGATE => {
                    try self.emitByte('-');
                    try self.emitExpression(un.operand, 12, false);
                },
                .BOOL_NOT => {
                    try self.emitByte('!');
                    try self.emitExpression(un.operand, 12, false);
                },
            },
            .FN_CALL => |call| {
                const fn_decl = self.ft.fn_decls.items[call.function];
                try self.emitMangledFnName(fn_decl);
                try self.emitByte('(');
                var arg_idx = call.args_start;
                var first = true;
                while (arg_idx != 0) {
                    if (!first)
                        try self.emitStr(", ");
                    first = false;
                    try self.emitExpression(arg_idx, 0, false);
                    arg_idx = self.ft.expressions.items[arg_idx].next_sibling;
                }
                try self.emitByte(')');
            },
            .STRUCT_INST => |si| {
                const sd = self.ft.struct_decls.items[si.struct_instance];
                try self.emitByte('(');
                try self.emitMangledStructName(sd);
                try self.emitStr("){ ");
                var arg_idx = si.args_start;
                var first = true;
                while (arg_idx != 0) {
                    if (!first)
                        try self.emitStr(", ");
                    first = false;
                    try self.emitExpression(arg_idx, 0, false);
                    arg_idx = self.ft.expressions.items[arg_idx].next_sibling;
                }
                try self.emitStr(" }");
            },
            .MEMBER_ACCESS => |ma| {
                try self.emitExpression(ma.base, 13, false);
                const base_type = self.ft.expressions.items[ma.base].type_;
                const sd = self.ft.struct_decls.items[base_type.structInstanceIdx()];
                const member = self.ft.var_decls.items[sd.members.start + ma.member_idx];
                try self.emitByte('.');
                try self.emitStr(member.name);
            },
            .ARRAY_ACCESS => |aa| {
                try self.emitExpression(aa.base, 14, false);
                var idx_expr = aa.indices_start;
                while (idx_expr != 0) {
                    try self.emitByte('[');
                    try self.emitExpression(idx_expr, 0, false);
                    try self.emitByte(']');
                    idx_expr = self.ft.expressions.items[idx_expr].next_sibling;
                }
            },
            .ARRAY_LIT => unreachable, // handled by emitArrayLitBody
            .FILL       => unreachable, // handled by emitArrayLitBody
            .INVALID    => unreachable,
        }
    }

    fn opPrecedence(op: BinaryOp) u8 {
        return switch (op) {
            .BOOL_OR => 2,
            .BOOL_AND => 3,
            .EQ, .NEQ, .BOOL_XOR => 5,
            .LT, .LE, .GT, .GE => 6,
            .ADD, .SUB => 9,
            .MUL, .DIV => 10,
            .POW => 13,
        };
    }

    fn opStr(op: BinaryOp) []const u8 {
        return switch (op) {
            .ADD => " + ",
            .SUB => " - ",
            .MUL => " * ",
            .DIV => " / ",
            .EQ => " == ",
            .NEQ => " != ",
            .LT => " < ",
            .LE => " <= ",
            .GT => " > ",
            .GE => " >= ",
            .BOOL_AND => " && ",
            .BOOL_OR => " || ",
            .BOOL_XOR => " != ",
            .POW => unreachable, // handled separately
        };
    }

    // ----- Printf for main -----

    fn emitPrintf(self: *CCodegen, result_type: DkType) EmitError!void {
        if (result_type == DkType.INT)
            try self.emitStr("printf(\"%ld\\n\", result);")
        else if (result_type == DkType.FLOAT)
            try self.emitStr("printf(\"%.6f\\n\", result);")
        else if (result_type == DkType.BOOL)
            try self.emitStr("printf(\"%s\\n\", result ? \"true\" : \"false\");")
        else
            unreachable;
    }

    // ----- Type names -----

    fn emitTypeName(self: *CCodegen, t: DkType) EmitError!void {
        if (t.isStruct()) {
            try self.emitMangledStructName(self.ft.struct_decls.items[t.structInstanceIdx()]);
            return;
        }
        if (t.isArray()) unreachable;
        if (t == DkType.INT)
            try self.emitStr("int64_t")
        else if (t == DkType.FLOAT)
            try self.emitStr("double")
        else if (t == DkType.BOOL)
            try self.emitStr("bool")
        else if (t == DkType.VOID)
            try self.emitStr("void")
        else
            unreachable;
    }

    // ----- Array helpers -----

    fn emitArrayDimSuffix(self: *CCodegen, ai: *const ft_ast.ArrayInstance, first_as_comment: bool) EmitError!void {
        for (0..ai.ndim) |i| {
            if (i == 0 and first_as_comment) {
                try self.w.print("[/*{d}*/]", .{ai.shape[i]});
            } else {
                try self.w.print("[{d}]", .{ai.shape[i]});
            }
        }
    }

    fn emitArrayLhs(self: *CCodegen, name: []const u8, outer_indices: []const IndexVal, last: IndexVal) EmitError!void {
        try self.emitStr(name);
        for (outer_indices) |idx| {
            try self.emitByte('[');
            switch (idx) {
                .literal  => |v| try self.w.print("{d}", .{v}),
                .loop_var => |d| try self.emitStr(loop_var_names[d]),
            }
            try self.emitByte(']');
        }
        try self.emitByte('[');
        switch (last) {
            .literal  => |v| try self.w.print("{d}", .{v}),
            .loop_var => |d| try self.emitStr(loop_var_names[d]),
        }
        try self.emitByte(']');
    }

    fn emitArrayLitBody(self: *CCodegen, name: []const u8, expr_idx: ExpressionIndex, indices: []IndexVal, dim: u8) EmitError!void {
        const expr = self.ft.expressions.items[expr_idx];
        const ai = &self.ft.array_instances.items[expr.type_.arrayInstanceIdx()];

        var non_fill_count: u16 = 0;
        {
            var ch = expr.kind.ARRAY_LIT.elems_start;
            while (ch != 0) {
                const ce = self.ft.expressions.items[ch];
                if (ce.kind != .FILL) non_fill_count += 1;
                ch = ce.next_sibling;
            }
        }
        const fill_count: u16 = ai.shape[0] - non_fill_count;

        var pos: u16 = 0;
        var ch = expr.kind.ARRAY_LIT.elems_start;
        while (ch != 0) {
            const ce = self.ft.expressions.items[ch];
            const next = ce.next_sibling;
            switch (ce.kind) {
                .FILL => |fill| {
                    const fill_start = pos;
                    const fill_end   = pos + fill_count;
                    const lv = loop_var_names[dim];
                    try self.emitIndent();
                    try self.w.print("for (int64_t {s} = {d}; {s} < {d}; {s}++) ", .{ lv, fill_start, lv, fill_end, lv });
                    const fve = self.ft.expressions.items[fill.value];
                    if (fve.kind == .ARRAY_LIT) {
                        try self.emitStr("{\n");
                        self.indent += 1;
                        var new_indices: []IndexVal = undefined;
                        new_indices.ptr = indices.ptr;
                        new_indices.len = indices.len + 1;
                        new_indices[indices.len] = .{ .loop_var = dim };
                        try self.emitArrayLitBody(name, fill.value, new_indices, dim + 1);
                        self.indent -= 1;
                        try self.emitIndent();
                        try self.emitStr("}\n");
                    } else {
                        try self.emitByte('\n');
                        self.indent += 1;
                        try self.emitIndent();
                        try self.emitArrayLhs(name, indices, .{ .loop_var = dim });
                        try self.emitStr(" = ");
                        try self.emitExpression(fill.value, 0, false);
                        try self.emitStr(";\n");
                        self.indent -= 1;
                    }
                    pos = fill_end;
                },
                .ARRAY_LIT => {
                    var new_indices: []IndexVal = undefined;
                    new_indices.ptr = indices.ptr;
                    new_indices.len = indices.len + 1;
                    new_indices[indices.len] = .{ .literal = pos };
                    try self.emitArrayLitBody(name, ch, new_indices, dim + 1);
                    pos += 1;
                },
                else => {
                    try self.emitIndent();
                    try self.emitArrayLhs(name, indices, .{ .literal = pos });
                    try self.emitStr(" = ");
                    try self.emitExpression(ch, 0, false);
                    try self.emitStr(";\n");
                    pos += 1;
                },
            }
            ch = next;
        }
    }

    fn emitArrayLitAssignment(self: *CCodegen, name: []const u8, array_type: DkType, rhs_expr: ExpressionIndex) EmitError!void {
        try self.emitArrayDecl(name, array_type);
        try self.emitIndent();
        try self.emitStr("{ // begin array literal assignment\n");
        self.indent += 1;
        var indices_buf: [ft_ast.MAX_ARRAY_NDIM]IndexVal = undefined;
        try self.emitArrayLitBody(name, rhs_expr, indices_buf[0..0], 0);
        self.indent -= 1;
        try self.emitIndent();
        try self.emitStr("} // end array literal assignment\n");
    }

    fn emitArrayDecl(self: *CCodegen, name: []const u8, array_type: DkType) EmitError!void {
        const ai = &self.ft.array_instances.items[array_type.arrayInstanceIdx()];
        try self.emitIndent();
        try self.emitTypeName(ai.elem_type);
        try self.emitByte(' ');
        try self.emitStr(name);
        try self.emitArrayDimSuffix(ai, false);
        try self.emitStr(";\n");
    }

    fn emitArrayParam(self: *CCodegen, name: []const u8, array_type: DkType) EmitError!void {
        const ai = &self.ft.array_instances.items[array_type.arrayInstanceIdx()];
        try self.emitStr("const ");
        try self.emitTypeName(ai.elem_type);
        try self.emitByte(' ');
        try self.emitStr(name);
        try self.emitArrayDimSuffix(ai, true);
    }

    // ----- Output helpers -----

    fn emitStr(self: *CCodegen, s: []const u8) EmitError!void {
        try self.w.writeAll(s);
    }

    fn emitByte(self: *CCodegen, b: u8) EmitError!void {
        try self.w.writeByte(b);
    }

    fn emitIndent(self: *CCodegen) EmitError!void {
        for (0..self.indent) |_|
            try self.emitStr("    ");
    }
};

// -----------------------------------------------------------------------
// Test utilities
// -----------------------------------------------------------------------

fn eqlIgnoringWhitespace(a: []const u8, b: []const u8) bool {
    var ai: usize = 0;
    var bi: usize = 0;
    while (true) {
        const a_ws = skipWsAndComments(a, ai);
        const b_ws = skipWsAndComments(b, bi);
        const a_had_ws = a_ws > ai;
        const b_had_ws = b_ws > bi;
        ai = a_ws;
        bi = b_ws;
        // Both at end: equal
        if (ai >= a.len and bi >= b.len) return true;
        // One at end but not the other: not equal
        if (ai >= a.len or bi >= b.len) return false;
        // Whitespace presence must match (but amount/type doesn't matter)
        if (a_had_ws != b_had_ws) return false;
        // Compare next non-ws char
        if (a[ai] != b[bi]) return false;
        ai += 1;
        bi += 1;
    }
}

fn skipWsAndComments(s: []const u8, start: usize) usize {
    var i = start;
    while (i < s.len) {
        if (s[i] == ' ' or s[i] == '\t' or s[i] == '\n' or s[i] == '\r') {
            i += 1;
        } else if (i + 1 < s.len and s[i] == '/' and s[i + 1] == '/') {
            // line comment: skip to end of line
            i += 2;
            while (i < s.len and s[i] != '\n') i += 1;
        } else if (i + 1 < s.len and s[i] == '/' and s[i + 1] == '*') {
            // block comment: skip to */
            i += 2;
            while (i + 1 < s.len and !(s[i] == '*' and s[i + 1] == '/')) i += 1;
            if (i + 1 < s.len) i += 2;
        } else break;
    }
    return i;
}

test "eqlIgnoringWhitespace" {
    try std.testing.expect(eqlIgnoringWhitespace("a b c", "a  b\tc"));
    try std.testing.expect(!eqlIgnoringWhitespace("a b c", "a  bc"));
    try std.testing.expect(eqlIgnoringWhitespace("a // comment\nb", "a\nb"));
    try std.testing.expect(eqlIgnoringWhitespace("a /* block */ b", "a b"));
    try std.testing.expect(!eqlIgnoringWhitespace("a b c", "abc"));
    try std.testing.expect(!eqlIgnoringWhitespace("abc", "abd"));
    try std.testing.expect(!eqlIgnoringWhitespace("abc", "ab"));
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const nr = @import("name_resolution.zig");
const type_inference = @import("type_inference.zig");
const elab = @import("ast_elaboration.zig");

const TokenStream = tok.TokenStream;

fn runCompilerAndCompareOutput(source: []const u8, expected_c: []const u8) !void {
    const gpa = std.testing.allocator;

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    try std.testing.expect(!pr.hasErrors());

    var elab_errors = try elab.elaborate(&pr.ast, ts.tokens, pr.root_node, gpa);
    defer elab_errors.deinit(gpa);

    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    try std.testing.expect(!di.hasErrors());

    var ft = try type_inference.infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());

    var aw: Writer.Allocating = .init(gpa);
    defer aw.deinit();
    try CCodegen.generate(&aw.writer, &ft);
    try aw.writer.flush();

    const c_out = aw.written();

    if (!eqlIgnoringWhitespace(c_out, expected_c)) {
        std.debug.print("=== EXPECTED ===\n{s}\n", .{expected_c});
        std.debug.print("=== GOT ===\n{s}\n", .{c_out});
        return error.OutputMismatch;
    }
}

test "generate C with correct oprerator precedence and associativity" {
    const source =
        \\fn main()
        \\    x := 3.0 / (4.0 / 5.0) / 6.0 / 7.0 * 2.3
        \\    result = (1 + 2) * 3 - 4 / 5 ** 6 - 4 
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\int main(void) {
        \\    int64_t result;
        \\    double x = 3e0 / (4e0 / 5e0) / 6e0 / 7e0 * 2.3e0;
        \\    result = (1 + 2) * 3 - 4 / (int64_t)pow(5, 6) - 4;
        \\    printf("%ld\n", result);
        \\    return 0;
        \\}
        \\
    ;

    try runCompilerAndCompareOutput(source, expected);
}

test "generate C for zinseszins program" {
    const source =
        \\fn do_zins(prev_val, zins)
        \\    result = prev_val * zins
        \\
        \\fn zinseszins(initial, zins, jahre)
        \\      result = initial
        \\      jahr := 0
        \\      while jahr < jahre
        \\         result = do_zins(result, zins)
        \\         jahr += 1
        \\
        \\fn main()
        \\    jahr := 0
        \\    zins := 1.02
        \\    result = zinseszins(1000.0, zins, 10)
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\double do_zins__Float__Float(double prev_val, double zins);
        \\double zinseszins__Float__Float__Int(double initial, double zins, int64_t jahre);
        \\
        \\double do_zins__Float__Float(double prev_val, double zins) {
        \\    double result;
        \\    result = prev_val * zins;
        \\    return result;
        \\}
        \\
        \\double zinseszins__Float__Float__Int(double initial, double zins, int64_t jahre) {
        \\    double result;
        \\    result = initial;
        \\    int64_t jahr = 0;
        \\    while (jahr < jahre) {
        \\        result = do_zins__Float__Float(result, zins);
        \\        jahr += 1;
        \\    }
        \\    return result;
        \\}
        \\
        \\int main(void) {
        \\    double result;
        \\    int64_t jahr = 0;
        \\    double zins = 1.02e0;
        \\    result = zinseszins__Float__Float__Int(1e3, zins, 10);
        \\    printf("%.6f\n", result);
        \\    return 0;
        \\}
        \\
    ;

    try runCompilerAndCompareOutput(source, expected);
}



test "generate C for non-generic struct" {
    const source =
        \\struct Point
        \\    x : Int = 0
        \\    y : Int = 0
        \\
        \\fn main()
        \\    p := Point(x=3, y=4)
        \\    result = p.x + p.y
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\typedef struct {
        \\    int64_t x;
        \\    int64_t y;
        \\} Point;
        \\
        \\int main(void) {
        \\    int64_t result;
        \\    Point p = (Point){ 3, 4 };
        \\    result = p.x + p.y;
        \\    printf("%ld\n", result);
        \\    return 0;
        \\}
        \\
    ;


    try runCompilerAndCompareOutput(source, expected);
}

test "generate C for nested structs" {
    const source =
        \\struct Vec2
        \\    x : Float = 0.0
        \\    y : Float = 0.0
        \\
        \\struct Line
        \\    start : Vec2
        \\    end : Vec2
        \\
        \\fn main()
        \\    a := Vec2(x=1.0, y=2.0)
        \\    b := Vec2(x=3.0, y=4.0)
        \\    l := Line(start=a, end=b)
        \\    result = l.start.x + l.end.y
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\typedef struct {
        \\    double x;
        \\    double y;
        \\} Vec2;
        \\
        \\typedef struct {
        \\    Vec2 start;
        \\    Vec2 end;
        \\} Line;
        \\
        \\int main(void) {
        \\    double result;
        \\    Vec2 a = (Vec2){ 1e0, 2e0 };
        \\    Vec2 b = (Vec2){ 3e0, 4e0 };
        \\    Line l = (Line){ a, b };
        \\    result = l.start.x + l.end.y;
        \\    printf("%.6f\n", result);
        \\    return 0;
        \\}
        \\
    ;


    try runCompilerAndCompareOutput(source, expected);
}

test "generate C for function taking struct parameter" {
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

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\typedef struct {
        \\    double x;
        \\    double y;
        \\} Point;
        \\
        \\double length__Point(Point p);
        \\
        \\double length__Point(Point p) {
        \\    double result;
        \\    result = p.x * p.x + p.y * p.y;
        \\    return result;
        \\}
        \\
        \\int main(void) {
        \\    double result;
        \\    Point pt = (Point){ 3e0, 4e0 };
        \\    result = length__Point(pt);
        \\    printf("%.6f\n", result);
        \\    return 0;
        \\}
        \\
    ;


    try runCompilerAndCompareOutput(source, expected);
}

test "generate C for function taking array parameter" {
    const source =
        \\fn sum3(a)
        \\    result = a[0] + a[1] + a[2]
        \\
        \\fn main()
        \\    a := [10, 20, 30]
        \\    result = sum3(a)
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\int64_t sum3__Arr_3_Int(const int64_t a[/*3*/]);
        \\
        \\int64_t sum3__Arr_3_Int(const int64_t a[/*3*/]) {
        \\    int64_t result;
        \\    result = a[0] + a[1] + a[2];
        \\    return result;
        \\}
        \\
        \\int main(void) {
        \\    int64_t result;
        \\    int64_t a[3];
        \\    { // begin array literal assignment
        \\        a[0] = 10;
        \\        a[1] = 20;
        \\        a[2] = 30;
        \\    } // end array literal assignment
        \\    result = sum3__Arr_3_Int(a);
        \\    printf("%ld\n", result);
        \\    return 0;
        \\}
        \\
    ;

    try runCompilerAndCompareOutput(source, expected);
}

test "generate C for 1D array with fill" {
    const source =
        \\fn main()
        \\    a : [4]Int = [1, 0..., 2]
        \\    result = a[0]
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\int main(void) {
        \\    int64_t result;
        \\    int64_t a[4];
        \\    { // begin array literal assignment
        \\        a[0] = 1;
        \\        for (int64_t _dk_i0 = 1; _dk_i0 < 3; _dk_i0++)
        \\            a[_dk_i0] = 0;
        \\        a[3] = 2;
        \\    } // end array literal assignment
        \\    result = a[0];
        \\    printf("%ld\n", result);
        \\    return 0;
        \\}
        \\
    ;

    try runCompilerAndCompareOutput(source, expected);
}

test "generate C for 2D array literal" {
    const source =
        \\fn main()
        \\    a := [[1, 2], [3, 4]]
        \\    result = a[0, 0]
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\int main(void) {
        \\    int64_t result;
        \\    int64_t a[2][2];
        \\    { // begin array literal assignment
        \\        a[0][0] = 1;
        \\        a[0][1] = 2;
        \\        a[1][0] = 3;
        \\        a[1][1] = 4;
        \\    } // end array literal assignment
        \\    result = a[0][0];
        \\    printf("%ld\n", result);
        \\    return 0;
        \\}
        \\
    ;

    try runCompilerAndCompareOutput(source, expected);
}

test "generate C for 3D array literal" {
    const source =
        \\fn main()
        \\    a : [3,2,4]Int = [[[1, 2...,], [3...,4]..., [5...]]..., [[6...]...]]
        \\    result = a[0, 0, 0]
    ;

    const expected =
        \\#include <stdint.h>
        \\#include <stdbool.h>
        \\#include <math.h>
        \\#include <stdio.h>
        \\
        \\int main(void) {
        \\    int64_t result;
        \\    int64_t a[3][2][4];
        \\    { // begin array literal assignment
        \\        for (int64_t _dk_i0 = 0; _dk_i0 < 2; _dk_i0++) {
        \\            a[_dk_i0][0][0] = 1;
        \\            for (int64_t _dk_i2 = 1; _dk_i2 < 3; _dk_i2++) 
        \\                a[_dk_i0][0][_dk_i2] = 2;
        \\            for (int64_t _dk_i1 = 1; _dk_i1 < 2; _dk_i1++) {
        \\                for (int64_t _dk_i2 = 0; _dk_i2 < 2; _dk_i2++) 
        \\                    a[_dk_i0][_dk_i1][_dk_i2] = 3;
        \\                a[_dk_i0][_dk_i1][2] = 4;
        \\            }
        \\            for (int64_t _dk_i2 = 0; _dk_i2 < 3; _dk_i2++) 
        \\                a[_dk_i0][2][_dk_i2] = 5;
        \\        }
        \\        for (int64_t _dk_i1 = 0; _dk_i1 < 2; _dk_i1++) {
        \\            for (int64_t _dk_i2 = 0; _dk_i2 < 3; _dk_i2++) 
        \\                a[2][_dk_i1][_dk_i2] = 6;
        \\        }
        \\    } // end array literal assignment
        \\    result = a[0][0][0];
        \\    printf("%ld\n", result);
        \\    return 0;
        \\}
        \\
    ;

    try runCompilerAndCompareOutput(source, expected);
}
