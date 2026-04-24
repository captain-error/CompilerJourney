const std = @import("std");
const util = @import("util.zig");
const type_inference = @import("type_inference.zig");

const DkType = type_inference.DkType;

const ScopeIndex = u29;
const StamentIndex = u31;
const ExpressionIndex = u32;
const VarDeclIndex = u30;
const FnDeclIndex = u28;

fn IndexRange(IndexType : type) type {
    return struct {
        start: IndexType = 0,
        end: IndexType = 0,
    };
}


const Scope = struct {
    kind: enum(u8) { INVALID = 0, FN, IF, ELSE, WHILE } = .INVALID,
    parent_scope: ScopeIndex = 0,
    first_statement: StamentIndex = 0, // linked list of statements in this scope (linked via next_sibling)
    first_decl: VarDeclIndex = 0, // linked list of variable declarations in this scope (linked via next_sibling)
};

const VarDecl = struct {
    name: []const u8 = &{}, // arena allocated
    type_: DkType = .INVALID,
    kind: enum(u8) { VAR, RESULT, FN_PARAM },
    parent_scope: ScopeIndex = 0,
    next_sibling: VarDeclIndex = 0,
};

const FnDecl = struct {
    name: []const u8 = &{}, // arena allocated
    return_type: DkType = .INVALID,
    params: IndexRange(VarDeclIndex) = .{},
    body_scope: ScopeIndex = 0,
};

const Expression = struct {
    type_: DkType = .INVALID,
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
            op: enum {
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
            },
            lhs: ExpressionIndex,
            rhs: ExpressionIndex,
        },
        UNARY_OP: struct {
            op: enum {
                NEGATE,
                BOOL_NOT,
            },
            operand: ExpressionIndex,
        },
        FN_CALL: struct {
            function: FnDeclIndex,
            args_start: ExpressionIndex, // linked list of argument expressions (linked via next_sibling)
        },
    } = .INVALID,
};

const Statement = struct {
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
            kind: enum { ASSIGN, PLUS, MINUS, MULT, DIV },
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

const FtAst = struct {
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
            .scopes            = .initCapacity(gpa, 1024),
            .fn_decls         = .initCapacity(gpa, 256),   
            .var_decls       = .initCapacity(gpa, 1024),
            .statements    = .initCapacity(gpa, 1024),
            .expressions  = .initCapacity(gpa, 1024),
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
