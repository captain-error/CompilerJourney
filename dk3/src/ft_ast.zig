const std = @import("std");
const util = @import("util.zig");
const type_inference = @import("type_inference.zig");

const DkType = type_inference.DkType;

pub const BinaryOp = enum {
    ADD,
    SUB,
    MUL,
    DIV,
    POW, //
    EQ,
    NEQ,
    LT,
    LE,
    GT,
    GE, //
    BOOL_AND,
    BOOL_OR,
    BOOL_XOR, //
};

pub const UnaryOp = enum {
    NEGATE,
    BOOL_NOT,
};

pub const AssignmentKind = enum {
    ASSIGN,
    PLUS,
    MINUS,
    MULT,
    DIV,
};

pub const ScopeIndex = u29;
pub const StamentIndex = u31;
pub const ExpressionIndex = u32;
pub const VarDeclIndex = u30;
pub const FnDeclIndex = u28;

pub fn IndexRange(IndexType: type) type {
    return struct {
        start: IndexType = 0,
        end: IndexType = 0,
    };
}

pub const Scope = struct {
    kind: enum(u8) { INVALID = 0, FN, IF, ELSE, WHILE } = .INVALID,
    parent_scope: ScopeIndex = 0,
    first_statement: StamentIndex = 0, // linked list of statements in this scope (linked via next_sibling)
    first_decl: VarDeclIndex = 0, // linked list of variable declarations in this scope (linked via next_sibling)
};

pub const VarDecl = struct {
    name: []const u8 = "", // arena allocated
    type_: DkType = .UNKNOWN,
    kind: enum(u8) { VAR, RESULT, FN_PARAM } = .VAR,
    parent_scope: ScopeIndex = 0,
    next_sibling: VarDeclIndex = 0,
};

pub const FnDecl = struct {
    name: []const u8 = "", // arena allocated
    return_type: DkType = .UNKNOWN,
    params: IndexRange(VarDeclIndex) = .{},
    body_scope: ScopeIndex = 0,
};

pub const Expression = struct {
    type_: DkType = .UNKNOWN,
    next_sibling: ExpressionIndex = 0,
    kind: union(enum) {
        INVALID: void,
        VAR_REF: VarDeclIndex,
        LITERAL: union(enum) {
            // TODO: add formatting info for literals (e.g. hex vs decimal vs bin int literals)
            int: i64,
            float: f64,
            boolean: bool,
        },
        BINARY_OP: struct {
            op: BinaryOp,
            lhs: ExpressionIndex,
            rhs: ExpressionIndex,
        },
        UNARY_OP: struct {
            op: UnaryOp,
            operand: ExpressionIndex,
        },
        FN_CALL: struct {
            function: FnDeclIndex,
            args_start: ExpressionIndex, // linked list of argument expressions (linked via next_sibling)
        },
    } = .INVALID,
};

pub const Statement = struct {
    next_sibling: StamentIndex = 0,
    kind: union(enum) {
        INVALID: void,
        VAR_DECL: struct {
            var_decl_idx: VarDeclIndex,
            rhs: ExpressionIndex,
        },
        ASSIGNMENT: struct {
            var_decl_idx: VarDeclIndex,
            type_: DkType,
            kind: AssignmentKind,
            rhs: ExpressionIndex,
        },
        RESULT_ASSIGN: struct {
            type_: DkType,
            rhs: ExpressionIndex,
        },
        IF_STMT: struct {
            condition: ExpressionIndex,
            then_scope: ScopeIndex = 0,
            else_scope: ScopeIndex = 0, // 0 if absent
        },
        WHILE_LOOP: struct {
            condition: ExpressionIndex,
            body_scope: ScopeIndex = 0,
        },
        FN_CALL: ExpressionIndex,
    } = .INVALID,
};

pub const FtAst = struct {
    // zig fmt: off
    fn_decls:     std.ArrayList(FnDecl),
    scopes:       std.ArrayList(Scope),
    var_decls:    std.ArrayList(VarDecl),
    statements:   std.ArrayList(Statement),
    expressions:  std.ArrayList(Expression),

    gpa:   std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    // zig fmt: on

    pub fn init(
        gpa: std.mem.Allocator,
    ) !FtAst {
        // zig fmt: off
        var res = FtAst{
            .gpa = gpa,
            .arena = .init(gpa),
            .scopes      = try .initCapacity(gpa, 1024),
            .fn_decls    = try .initCapacity(gpa, 256),
            .var_decls   = try .initCapacity(gpa, 1024),
            .statements  = try .initCapacity(gpa, 1024),
            .expressions = try .initCapacity(gpa, 1024),
        };

        res.scopes     .appendAssumeCapacity(.{}); // reserve index 0 for invalid scope  
        res.fn_decls   .appendAssumeCapacity(.{}); // reserve index 0 for invalid fn decl
        res.var_decls  .appendAssumeCapacity(.{}); // reserve index 0 for invalid var decl
        res.statements .appendAssumeCapacity(.{}); // reserve index 0 for invalid statement
        res.expressions.appendAssumeCapacity(.{}); // reserve index 0 for invalid expression
        // zig fmt: on

        return res;
    }

    pub fn deinit(self: *FtAst) void {
        // zig fmt: off
        self.scopes      .deinit(self.gpa);
        self.fn_decls    .deinit(self.gpa);
        self.var_decls   .deinit(self.gpa);
        self.statements  .deinit(self.gpa);
        self.expressions .deinit(self.gpa);
        // zig fmt: on
        self.arena.deinit();
    }
};
