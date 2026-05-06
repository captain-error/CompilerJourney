const std = @import("std");
const util = @import("util.zig");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");

const TokenIndex = tok.TokenStream.TokenIndex;
const AstNodeIndex = par.AstNodeIndex;

pub const DkType = enum(u16) {
    UNKNOWN = 0,
    VOID,
    BOOL,
    INT,
    FLOAT,
    ANY,
    ERROR,
    _,

    const FIRST_STRUCT: u16 = @intFromEnum(DkType.ERROR) + 1;

    pub fn isStruct(self: DkType) bool {
        return @intFromEnum(self) >= FIRST_STRUCT;
    }

    pub fn structInstanceIdx(self: DkType) StructInstanceIndex {
        return @intCast(@intFromEnum(self) - FIRST_STRUCT);
    }

    pub fn fromStructInstance(idx: StructInstanceIndex) DkType {
        return @enumFromInt(@as(u16, @intCast(idx)) + FIRST_STRUCT);
    }

    pub fn langName(self: DkType) []const u8 {
        return switch (self) {
            .UNKNOWN => "Unknown",
            .VOID => "Void",
            .BOOL => "Bool",
            .INT => "Int",
            .FLOAT => "Float",
            .ANY => "Any",
            .ERROR => "Error",
            _ => "Struct", // caller should use StructDecl.name for struct types
        };
    }
};

pub const StructInstanceIndex = u16;

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

pub const StructDecl = struct {
    name: []const u8 = "", // arena allocated, unmangled template name
    members: IndexRange(VarDeclIndex) = .{}, // member name + concrete type (reuses VarDecl)
    generic: bool = false,
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
        STRUCT_INST: struct {
            struct_instance: StructInstanceIndex,
            args_start: ExpressionIndex, // member values in member order, linked list
        },
        MEMBER_ACCESS: struct {
            base: ExpressionIndex,
            member_idx: u8,
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
        MEMBER_ASSIGN: struct {
            base: ExpressionIndex,
            member_idx: u8,
            kind: AssignmentKind,
            rhs: ExpressionIndex,
        },
        FN_CALL: ExpressionIndex,
    } = .INVALID,
};

pub const TypeError = union(enum) {
    return_type_missing_for_recursive_fn: AstNodeIndex,
    fn_params_are_immutable: struct {
        declaration: TokenIndex,
        usage: TokenIndex,
    },
    wrong_num_fun_args: struct {
        ast_node: AstNodeIndex,
        expected: u8,
        actual: u8,
    },
    wrong_type: struct {
        ast_node: AstNodeIndex,
        expected: [:0]const u8,
        actual: DkType,
    },
    type_mismatch: struct {
        ast_node: AstNodeIndex,
        lhs: DkType,
        rhs: DkType,
    },
    result_type_mismatch: struct {
        ast_node: AstNodeIndex,
        lhs: DkType,
        rhs: DkType,
    },
    too_many_positional_args: struct {
        ast_node: AstNodeIndex,
        expected: u8,
        actual: u8,
    },
    unknown_named_arg: struct {
        ast_node: AstNodeIndex,
        member_name: TokenIndex,
    },
    duplicate_named_arg: struct {
        ast_node: AstNodeIndex,
        member_name: TokenIndex,
    },
    missing_required_arg: struct {
        ast_node: AstNodeIndex,
        member_name: TokenIndex,
    },
    member_access_on_non_struct: struct {
        ast_node: AstNodeIndex,
        actual: DkType,
    },
    unknown_field: struct {
        ast_node: AstNodeIndex,
        field_name: TokenIndex,
    },
};

pub const TypeErrorInfo = struct {
    error_: TypeError,
    fn_instantiation_stack_top: [MAX_FN_INSTANTIATION_REPORT_DEPTH]InstantiationInfo,
    fn_instantiation_stack_len: usize,

    pub const InstantiationInfo = struct {
        function: usize,
        fn_call_ast_idx: AstNodeIndex,
    };

    pub const MAX_FN_INSTANTIATION_REPORT_DEPTH = 16;
};

pub const FtAst = struct {
    // zig fmt: off
    fn_decls:     std.ArrayList(FnDecl),
    struct_decls: std.ArrayList(StructDecl),
    scopes:       std.ArrayList(Scope),
    var_decls:    std.ArrayList(VarDecl),
    statements:   std.ArrayList(Statement),
    expressions:  std.ArrayList(Expression),
    errors:       std.ArrayList(TypeErrorInfo),

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
            .struct_decls = try .initCapacity(gpa, 64),
            .var_decls   = try .initCapacity(gpa, 1024),
            .statements  = try .initCapacity(gpa, 1024),
            .expressions = try .initCapacity(gpa, 1024),
            .errors      = try .initCapacity(gpa, 8),
        };

        res.scopes     .appendAssumeCapacity(.{}); // reserve index 0 for invalid scope
        res.fn_decls   .appendAssumeCapacity(.{}); // reserve index 0 for invalid fn decl
        res.var_decls  .appendAssumeCapacity(.{}); // reserve index 0 for invalid var decl
        res.statements .appendAssumeCapacity(.{}); // reserve index 0 for invalid statement
        res.expressions.appendAssumeCapacity(.{}); // reserve index 0 for invalid expression
        // zig fmt: on

        return res;
    }

    pub fn hasErrors(self: *const FtAst) bool {
        return self.errors.items.len > 0;
    }

    pub fn deinit(self: *FtAst) void {
        // zig fmt: off
        self.scopes      .deinit(self.gpa);
        self.fn_decls    .deinit(self.gpa);
        self.struct_decls.deinit(self.gpa);
        self.var_decls   .deinit(self.gpa);
        self.statements  .deinit(self.gpa);
        self.expressions .deinit(self.gpa);
        self.errors      .deinit(self.gpa);
        // zig fmt: on
        self.arena.deinit();
    }
};
