const std = @import("std");
const tok = @import("tokenizer.zig");
const treemod = @import("tree.zig");
// const exdat = @import("extra_data.zig");

const par = @This();

const assert = std.debug.assert;

const Token = tok.Token;
const Tokenizer = tok.Tokenizer;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;

pub const AST = treemod.GenericTree(AstNode);
pub const AstNodeIndex = treemod.NodeIndex;

pub const AstNode = struct {
    tag: AstNodeTag = .INVALID,
    token_index: TokenIndex = 0,

    first_child: AstNodeIndex = 0,
    next_sibling: AstNodeIndex = 0,

    pub fn hasChild(node: AstNode) bool {
        return node.first_child > 0;
    }

    pub fn children(node: AstNode, ast: *const AST) AST.LinkedList {
        return ast.getList(node.first_child);
    }

    // convenience methods for certain node kinds:
    pub const ConditionThenElse = struct { cond_idx: AstNodeIndex, then_idx: AstNodeIndex, else_idx: AstNodeIndex };
    pub fn conditionThenElse(node: AstNode, ast: *const AST) ConditionThenElse {
        assert(node.tag == .IF or node.tag == .WHILE);

        const cond_idx = node.first_child;
        const then_idx = ast.get(cond_idx).next_sibling;
        const else_idx = ast.get(then_idx).next_sibling;

        assert(cond_idx != 0);
        assert(then_idx != 0);

        return .{ .cond_idx = cond_idx, .then_idx = then_idx, .else_idx = else_idx };
    }

    pub fn then(node: AstNode, ast: *const AST) AstNodeIndex {
        assert(node.tag == .IF);
        const then_idx = ast.get(node.first_child).next_sibling;
        assert(then_idx != 0);
        return then_idx;
    }
};

pub const AstNodeTag = enum(u8) {
    INVALID = 0,
    DECLARATION, // token_index = declared IDENT. children: [optional TYPE, optional rhs_expr]
    ASSIGNMENT, // 2 children: lhs (ATOM or MEMBER_ACCESS), rhs
    BINARY_OP, // 2 children: lhs, rhs
    UNARY_OP, // 1 child
    FNDECL, // 2 children: param list, body
    FNPARAMS, // arbitrary many children
    PARAM, // token_index = param name IDENT. children: [optional TYPE, optional default value]
    RETURN, // 1 child: return value (expression)
    CALL_OR_INST, // N children: positional exprs and/or NAMED_ARGs. Python-style: after first named, all must be named.
    NAMED_ARG, // token_index = field name IDENT. 1 child: value expression.
    WHILE, // 2 children: condition, body
    BLOCK, // arbitrary many children
    ATOM, // no child
    IF, // 3 children: condition, then, else
    STRUCTDECL, // token_index = struct name IDENT. children: MEMBER nodes
    MEMBER, // token_index = member name IDENT. 0..=2 children. (in struct decl: optional TYPE, optional default value.)
    TYPE, // token_index = type name IDENT. 0 children for scalar, 1 child (ARRAY_SHAPE) for array type.
    MEMBER_ACCESS, // token_index = field name IDENT. 1 child: object expression.
    ARRAY_LIT, // array literal [1,2,3]. arbitrary children: ATOM, ARRAY_LIT, FILL
    FILL, // fill element X... — 1 child: fill value expression
    ARRAY_SHAPE, // dimension list in array type — 1+ children: ATOM (size) or INFER_DIM
    INFER_DIM, // _ placeholder in array shape — 0 children
    ARRAY_ACCESS, // a[i,j] — 2 children: target expression + INDEX_ARGS
    INDEX_ARGS, // index argument list — 1+ children
};

pub const PrecedenceLevel = u8;

pub const ParseResult = struct {
    ast: AST,
    root_node: AstNodeIndex,
    errors: std.ArrayList(Parser.Error),
    gpa: std.mem.Allocator,

    pub fn hasErrors(self: *const ParseResult) bool {
        return self.errors.items.len > 0;
    }

    pub fn deinit(self: *ParseResult) void {
        self.ast.deinit();
        self.errors.deinit(self.gpa);
    }
};

pub fn parse(source: []const u8, tokens: []const Token, gpa: std.mem.Allocator) !ParseResult {
    var p = try Parser.init(source, tokens, gpa);
    errdefer p.deinit();

    _ = try p.parse();

    return .{
        .ast = p.ast,
        .root_node = p.root_node,
        .errors = p.errors,
        .gpa = gpa,
    };
}

const ParserError = error{
    OutOfMemory,
    IfConditionIsTooLong,
    InternalCompilerError,
};

pub const Parser = struct {
    source: []const u8,
    tokens: []const Token,
    ast: AST,
    root_node: AstNodeIndex = 0,
    errors: std.ArrayList(Error) = .empty,
    gpa: std.mem.Allocator,

    token_idx: TokenIndex,

    pub fn hasErrors(p: *const Parser) bool {
        return p.errors.items.len > 0;
    }

    pub const Error = struct {
        token_index: TokenIndex,
        expected: [:0]const u8,
    };

    const InternalParserError = error{
        OutOfMemory,
        IfConditionIsTooLong,
        UnexpectedToken, // only used internally,
        InternalCompilerError,
    };

    pub fn init(source: []const u8, tokens: []const Token, gpa: std.mem.Allocator) !Parser {
        std.debug.assert(tokens.len > 0);
        const approximate_ast_node_count = tokens.len + 1; // TODO: revise this
        return .{
            .ast = try .init(gpa, approximate_ast_node_count),
            .source = source,
            .tokens = tokens,
            .gpa = gpa,
            .token_idx = 1,
        };
    }

    pub fn deinit(p: *Parser) void {
        p.ast.deinit();
        p.errors.deinit(p.gpa);
    }

    fn peek(p: *const Parser, lookahead: u32) Token {
        const idx = @min(p.token_idx + lookahead, p.tokens.len - 1);
        return p.tokens[idx];
    }

    fn next(p: *Parser) void {
        if (p.token_idx >= p.tokens.len - 1)
            return;

        p.token_idx += 1;
    }

    fn skip(p: *Parser, tags : anytype) void {
        var skipped = true;

        while(skipped)        {
            skipped = false;
            inline for (tags) |tag| {
                if (p.peek(0).tag == tag) {
                    p.next();
                    skipped = true;
                }
            }
        }
    }

    // fn addNode(p: *Parser, node: AstNode) !AstNodeIndex {
    //     // std.debug.print("addNode: {any} token={any}\n", .{ node, p.tokens[node.token_index] });
    //     const index: AstNodeIndex = @intCast(p.ast.nodes.items.len);
    //     try p.ast_nodes.append(p.gpa, node);
    //     return index;
    // }

    // fn addExtra(p: *Parser, node_index: AstNodeIndex) !void {
    //     try p.extra.append(p.gpa, node_index);
    // }

    fn emitError(p: *Parser, token_index: TokenIndex, expected: [:0]const u8) !void {
        const err = try p.errors.addOne(p.gpa);
        err.token_index = token_index;
        err.expected = expected;
    }

    fn filterErr(val: InternalParserError!AstNodeIndex) ParserError!AstNodeIndex {
        return val catch |err| switch (err) {
            error.UnexpectedToken => return 0, // add INVALID ast node. continue parsing.
            else => |e| return e,
        };
    }

    fn advanceTillEoStatementOnErr(p: *Parser, val: InternalParserError!AstNodeIndex) ParserError!AstNodeIndex {
        if (val) |v| {
            return v;
        } else |err| {
            switch (err) {
                error.UnexpectedToken => {
                    p.advanceTillEoStatement();
                    return 0;
                },
                else => |e| return e,
            }
        }
    }

    pub fn advanceTillEoStatement(p: *Parser) void {
        while (true) {
            switch (p.peek(0).tag) {
                .EOF, .EOL, .SEMICOLON => return,
                else => p.next(),
            }
        }
    }

    pub fn advanceTillEoBlock(p: *Parser) void {
        while (true) {
            switch (p.peek(0).tag) {
                .EOF, .END => return,
                else => p.next(),
            }
        }
    }

    fn precedenceOf(tag: Token.Tag) PrecedenceLevel {
        return switch (tag) {
            .OR => 1,
            .XOR => 2,
            .AND => 3,
            .GT, .GE, .LT, .LE => 4,
            .PLUS, .MINUS => 5,
            .TIMES, .DIV => 6,
            .POW => 7,
            else => 0,
        };
    }

    fn isRightAssociative(tag: Token.Tag) bool {
        return switch(tag) {
            .POW => true,
            else => false,
        };
    }

    fn expectToken(p: *Parser, tag: Token.Tag) ParserError!bool {
        if (p.peek(0).tag != tag) {
            try p.emitError(p.token_idx, switch (tag) {
                inline else => |t| @tagName(t),
            });
            return false;
        }

        return true;
    }

    fn expectOneOf(p: *Parser, comptime token_tags: anytype, expetation_msg: [:0]const u8) ParserError!bool {
        const actual_tag = p.peek(0).tag;
        // std.debug.print("expectOneOf({any}): actual: {any}\n", .{ token_tags, actual_tag });
        inline for (token_tags) |tag| {
            if (actual_tag == tag)
                return true;
        }
        try p.emitError(p.token_idx, expetation_msg);
        return false;
    }

    fn expectEndOfStatement(p: *Parser) ParserError!bool {
        switch (p.peek(0).tag) {
            .END_BLOCK, .EOL, .SEMICOLON, .EOF => return true,
            else => {
                try p.emitError(p.token_idx, "semicolon or end-of-line");
                return false;
            },
        }
    }

    // fn parseAtom(p: *Parser) InternalParserError!AstNodeIndex {
    //     // std.debug.print("parseAtom: start: {s}\n", .{@tagName(p.peek(0).tag)});

    //     if (!try p.expectOneOf(
    //         .{
    //             .FLOAT_LIT,
    //             .INT_LIT,
    //             .TRUE,
    //             .FALSE,
    //             .IDENTIFIER,
    //         },
    //         "literal or variable",
    //     )) return error.UnexpectedToken;

    //     const idx = p.token_idx;
    //     p.next();

    //     return p.addNode(.{
    //         .token_index = idx,
    //         .tag = .ATOM,
    //     });
    // }

    pub fn parseFnDecl(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .FN);
        p.next();

        if (!try p.expectToken(.IDENTIFIER)) 
            return error.UnexpectedToken;
        
        const name_token_idx = p.token_idx;
        p.next();

        if (!try p.expectToken(.LPAREN))
             return error.UnexpectedToken;
       

        const node_idx = try p.ast.append(.{
            .tag = .FNDECL,
            .token_index = name_token_idx,
        });
    
        const params_idx = try p.ast.append(.{
            .tag = .FNPARAMS,
            .token_index = p.token_idx, // not really needed, points to the opening parenthesis token
        });
        p.ast.get(node_idx).first_child = params_idx;

        p.next();
        var param_list = p.ast.startList();

        // while(true) {
        //     switch(p.peek(0).tag) {
        //         .EOL, .BEGIN_BLOCK, .END_BLOCK => {
        //             p.next();
        //             continue;
        //         },
        //         else => break,
        //     }
        // }

        while (p.peek(0).tag != .RPAREN) {
            // FIXME! only allow balanced BEGIN_BLOCK/END_BLOCK

            p.skip(.{ .EOL });
            
            if (!try p.expectToken(.IDENTIFIER)) 
                return error.UnexpectedToken;
            const param_idx = try p.parseStructMemberOrParam(.PARAM);
            try param_list.appendExisting(param_idx);

            p.skip(.{ .EOL });

            if (p.peek(0).tag != .COMMA) break;
            p.next();
        }

        if (!try p.expectToken(.RPAREN))
            return error.UnexpectedToken;
        p.next();

        if (!try p.expectToken(.EOL))
            return error.UnexpectedToken;
        p.next();

        const body_idx = try p.parseIndentedCodeBlock();
        const params_node = p.ast.get(params_idx);

        params_node.first_child = param_list.start_index;
        params_node.next_sibling = body_idx;

        return node_idx;
    }

    pub fn parseReturn(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .RETURN);
        const node_idx = try p.ast.append(.{
            .tag = .RETURN,
            .token_index = p.token_idx,
        });
        p.next();

        const expr_idx = try p.parseExpression();
        p.ast.get(node_idx).first_child = expr_idx;

        if (!try p.expectEndOfStatement())
            return error.UnexpectedToken;
        p.next();

        return node_idx;
    }

    pub fn parseAtomOrFuncall(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseAtomOrFuncall: start: {s}\n", .{@tagName(p.peek(0).tag)});

        if (!try p.expectOneOf(
            .{
                .FLOAT_LIT,
                .INT_LIT,
                .TRUE,
                .FALSE,
                .IDENTIFIER,
            },
            "literal, variable or function call",
        )) return error.UnexpectedToken;

        var result: AstNodeIndex = undefined;

        if (p.peek(0).tag == .IDENTIFIER and p.peek(1).tag == .LPAREN) {
            result = try p.parseCallOrInst();
        } else {
            const idx = p.token_idx;
            p.next();
            result = try p.ast.append(.{
                .token_index = idx,
                .tag = .ATOM,
            });
        }

        // Postfix member access chain
        result = try p.parseMemberAccessChain(result);
        // Postfix array access
        if (p.peek(0).tag == .LBRACKET)
            result = try p.parseArrayAccess(result);
        return result;
    }

    fn parseMemberAccessChain(p: *Parser, base: AstNodeIndex) InternalParserError!AstNodeIndex {
        var result = base;
        while (p.peek(0).tag == .DOT) {
            p.next(); // consume DOT
            if (!try p.expectToken(.IDENTIFIER))
                return error.UnexpectedToken;

            const field_token = p.token_idx;
            p.next();

            const access_idx = try p.ast.append(.{
                .tag = .MEMBER_ACCESS,
                .token_index = field_token,
                .first_child = result,
            });
            result = access_idx;
        }
        return result;
    }

    pub fn parseStructDecl(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .STRUCT);
        p.next();

        if (!try p.expectToken(.IDENTIFIER))
            return error.UnexpectedToken;

        const name_token_idx = p.token_idx;
        p.next();

        if (!try p.expectToken(.EOL))
            return error.UnexpectedToken;
        p.next();

        const node_idx = try p.ast.append(.{
            .tag = .STRUCTDECL,
            .token_index = name_token_idx,
        });

        if (!try p.expectToken(.BEGIN_BLOCK))
            return error.UnexpectedToken;
        p.next();

        var member_list = p.ast.startList();

        member_loop: while (true) {
            assert(p.peek(0).tag != .EOF);
            // p.peek(0).tag != .END_BLOCK
            switch (p.peek(0).tag) {
                .END_BLOCK => {
                    p.next();
                    break :member_loop;
                },
                .EOL, .SEMICOLON => {
                    p.next();
                    continue :member_loop;
                },
                .IDENTIFIER => {
                    const member_idx = try p.parseStructMemberOrParam(.MEMBER);
                    try member_list.appendExisting(member_idx);
                    if (!try p.expectEndOfStatement())
                        return error.UnexpectedToken;
                    p.next();
                },
                else => {
                    try p.emitError(p.token_idx, "struct member declaration");
                    p.advanceTillEoStatement();
                    continue :member_loop;
                },
            }
        }

        p.ast.get(node_idx).first_child = member_list.start_index;
        return node_idx;
    }

    ///   x                --> MEMBER/PARAM(x)
    ///   x : = expr       --> MEMBER/PARAM(x), children: [expr]
    ///   x : Type = expr  --> MEMBER/PARAM(x), children: [TYPE(Type), expr]
    ///   x : Type         --> MEMBER/PARAM(x), children: [TYPE(Type)]
    fn parseStructMemberOrParam(p: *Parser, tag : AstNodeTag) InternalParserError!AstNodeIndex {
        if (!try p.expectToken(.IDENTIFIER))
            return error.UnexpectedToken;

        const ident_token_idx = p.token_idx;
        p.next();

        const node_idx = try p.ast.append(.{
            .tag = tag,
            .token_index = ident_token_idx,
        });

        // improve error message for missing colon:
        if (p.peek(0).tag == .ASSIGN) {
            try p.emitError(p.token_idx, "':'");
            return error.UnexpectedToken;
        }
        
        if (p.peek(0).tag == .COLON) {
            p.next();
            switch (p.peek(0).tag) {
                .ASSIGN => {
                    // x : = expr (no type)
                    p.next();
                    const rhs = try p.parseExpression();
                    p.ast.get(node_idx).first_child = rhs;
                },
                .IDENTIFIER, .LBRACKET => {
                    // x : Type ...
                    const type_idx = try p.parseType();
                    p.ast.get(node_idx).first_child = type_idx;

                    if (p.peek(0).tag == .ASSIGN) {
                        // x : Type = expr
                        p.next();
                        const rhs = try p.parseExpression();
                        p.ast.get(type_idx).next_sibling = rhs;
                    }
                },
                else => {
                    try p.emitError(p.token_idx, "type name or '='");
                    return error.UnexpectedToken;
                },
            }
        }

        return node_idx;
    }

    fn parseScalarType(p: *Parser) InternalParserError!AstNodeIndex {
        if (!try p.expectToken(.IDENTIFIER))
            return error.UnexpectedToken;
        const type_idx = try p.ast.append(.{ .tag = .TYPE, .token_index = p.token_idx });
        p.next();
        return type_idx;
    }

    fn parseArrayType(p: *Parser) InternalParserError!AstNodeIndex {
        assert(p.peek(0).tag == .LBRACKET);
        const bracket_token = p.token_idx;
        p.next(); // consume [

        const shape_idx = try p.ast.append(.{ .tag = .ARRAY_SHAPE, .token_index = bracket_token });
        var dim_list = p.ast.startList();

        while (p.peek(0).tag != .RBRACKET) {
            if (p.peek(0).tag == .EOF) {
                try p.emitError(p.token_idx, "']'");
                return error.UnexpectedToken;
            }
            const dim_idx = blk: {
                if (p.peek(0).tag == .IDENTIFIER and
                    std.mem.eql(u8, p.source[p.tokens[p.token_idx].start..p.tokens[p.token_idx].end()], "_"))
                {
                    const idx = try p.ast.append(.{ .tag = .INFER_DIM, .token_index = p.token_idx });
                    p.next();
                    break :blk idx;
                } else {
                    if (!try p.expectToken(.INT_LIT))
                        return error.UnexpectedToken;
                    const idx = try p.ast.append(.{ .tag = .ATOM, .token_index = p.token_idx });
                    p.next();
                    break :blk idx;
                }
            };
            try dim_list.appendExisting(dim_idx);
            if (p.peek(0).tag == .COMMA) p.next();
        }

        if (!try p.expectToken(.RBRACKET))
            return error.UnexpectedToken;
        p.next(); // consume ]

        p.ast.get(shape_idx).first_child = dim_list.start_index;

        const type_idx = try p.parseScalarType();
        p.ast.get(type_idx).first_child = shape_idx;
        return type_idx;
    }

    fn parseType(p: *Parser) InternalParserError!AstNodeIndex {
        if (p.peek(0).tag == .LBRACKET)
            return p.parseArrayType();
        return p.parseScalarType();
    }

    pub fn parseExpression(p: *Parser) InternalParserError!AstNodeIndex {
        // adapted from https://en.wikipedia.org/wiki/Operator-precedence_parser
        
        const lhs = try p.parseSubExpr();
        return p.parseExpression1(lhs, 1);
    }

    fn parseExpression1(p: *Parser, lhs_: AstNodeIndex, min_precedence: PrecedenceLevel) InternalParserError!AstNodeIndex {
        assert(min_precedence > 0);
        var lhs = lhs_;
        while (true) {
            const op_precedence = precedenceOf(p.peek(0).tag); // will return 0 if the token is not an operator
            if(op_precedence < min_precedence) // FIXME for == and != and which can only apear once (cannot be chained) this should be "<=" instead of "<"
                break;
            const op = p.token_idx;
            p.next();
            var rhs = try p.parseSubExpr();

            while(true) {
                const precedence = precedenceOf(p.peek(0).tag);
                if (precedence <= op_precedence and ! (precedence == op_precedence and isRightAssociative(p.peek(0).tag)))
                    break;
                rhs = try p.parseExpression1(rhs, op_precedence + @intFromBool(precedence > op_precedence));
            }
            const binop_idx = try p.ast.append(.{
                .tag = .BINARY_OP,
                .token_index = op,
                .first_child = lhs,
            });
            p.ast.get(lhs).next_sibling = rhs;
            lhs = binop_idx;
            
        }

        return lhs;
    }


    fn parseArrayLiteral(p: *Parser) InternalParserError!AstNodeIndex {
        assert(p.peek(0).tag == .LBRACKET);
        const bracket_token = p.token_idx;
        p.next(); // consume [

        const lit_idx = try p.ast.append(.{ .tag = .ARRAY_LIT, .token_index = bracket_token });
        var elem_list = p.ast.startList();

        while (p.peek(0).tag != .RBRACKET) {
            if (p.peek(0).tag == .EOF) {
                try p.emitError(p.token_idx, "']'");
                return error.UnexpectedToken;
            }
            var elem_idx: AstNodeIndex = undefined;
            if (p.peek(0).tag == .LBRACKET) {
                elem_idx = try p.parseArrayLiteral();
            } else {
                elem_idx = try p.parseExpression();
            }
            if (p.peek(0).tag == .ELLIPSIS) {
                const fill_idx = try p.ast.append(.{ .tag = .FILL, .token_index = p.token_idx, .first_child = elem_idx });
                p.next(); // consume ...
                elem_idx = fill_idx;
            }
            try elem_list.appendExisting(elem_idx);
            if (p.peek(0).tag == .COMMA) p.next();
        }

        if (!try p.expectToken(.RBRACKET))
            return error.UnexpectedToken;
        p.next(); // consume ]

        p.ast.get(lit_idx).first_child = elem_list.start_index;
        return lit_idx;
    }

    fn parseArrayAccess(p: *Parser, target: AstNodeIndex) InternalParserError!AstNodeIndex {
        assert(p.peek(0).tag == .LBRACKET);
        const bracket_token = p.token_idx;
        p.next(); // consume [

        const args_idx = try p.ast.append(.{ .tag = .INDEX_ARGS, .token_index = bracket_token });
        var idx_list = p.ast.startList();

        while (p.peek(0).tag != .RBRACKET) {
            if (p.peek(0).tag == .EOF) {
                try p.emitError(p.token_idx, "']'");
                return error.UnexpectedToken;
            }
            const idx = try p.parseExpression();
            try idx_list.appendExisting(idx);
            if (p.peek(0).tag == .COMMA) p.next();
        }

        if (!try p.expectToken(.RBRACKET))
            return error.UnexpectedToken;
        p.next(); // consume ]

        p.ast.get(args_idx).first_child = idx_list.start_index;

        const access_idx = try p.ast.append(.{ .tag = .ARRAY_ACCESS, .token_index = bracket_token, .first_child = target });
        p.ast.get(target).next_sibling = args_idx;
        return access_idx;
    }

    pub fn parseSubExpr(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.print("parseSubExpr: tag: {s}\n", .{@tagName(p.peek(0).tag)});
        var res: AstNodeIndex = undefined;
        switch (p.peek(0).tag) {
            .LBRACKET => {
                res = try p.parseArrayLiteral();
            },
            .LPAREN => {
                p.next();
                res = try p.parseExpression();
                if (!try p.expectToken(.RPAREN)) return error.UnexpectedToken;
                p.next();
            },
            .MINUS, .NOT => {
                res = try p.ast.append(.{
                    .tag = .UNARY_OP,
                    .token_index = p.token_idx,
                });
                p.next();
                p.ast.get(res).first_child = try p.parseSubExpr();
            },
            else => {
                res = try p.parseAtomOrFuncall();
            },
        }
        return res;
    }

    /// Parse a call/inst as a statement — handles trailing `.field = rhs` or end-of-statement.
    fn parseCallOrInstStatement(p: *Parser) InternalParserError!AstNodeIndex {
        const call_idx = try p.parseCallOrInst();
        // Check if this is actually a member assignment: foo(x).bar = ...
        if (p.peek(0).tag == .DOT) {
            const lhs = try p.parseMemberAccessChain(call_idx);
            if (!try p.expectOneOf(.{
                .ASSIGN,
                .PLUSASSIGN,
                .MINUSASSIGN,
                .MULTASSIGN,
                .DIVASSIGN,
            }, "assignment operator"))
                return error.UnexpectedToken;

            const op = p.token_idx;
            p.next();
            const rhs = try p.parseExpression();
            if (!try p.expectEndOfStatement())
                return error.UnexpectedToken;
            p.next();
            const node_idx = try p.ast.append(.{
                .tag = .ASSIGNMENT,
                .token_index = op,
                .first_child = lhs,
            });
            p.ast.get(lhs).next_sibling = rhs;
            return node_idx;
        }
        if (!try p.expectEndOfStatement())
            return error.UnexpectedToken;
        p.next();
        return call_idx;
    }

    pub fn parseCallOrInst(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .IDENTIFIER);
        const name_token = p.token_idx;
        p.next();

        std.debug.assert(p.peek(0).tag == .LPAREN);
        p.next();

        const node_idx = try p.ast.append(.{
            .tag = .CALL_OR_INST,
            .token_index = name_token,
        });

        var arg_list = p.ast.startList();
        var seen_named = false;

        while (p.peek(0).tag != .RPAREN) {
            // Check for named arg: IDENT ASSIGN
            if (p.peek(0).tag == .IDENTIFIER and p.peek(1).tag == .ASSIGN) {
                seen_named = true;
                const field_token = p.token_idx;
                p.next(); // consume IDENT
                p.next(); // consume ASSIGN

                const value_expr = try p.parseExpression();
                const named_arg_idx = try p.ast.append(.{
                    .tag = .NAMED_ARG,
                    .token_index = field_token,
                    .first_child = value_expr,
                });
                try arg_list.appendExisting(named_arg_idx);
            } else {
                if (seen_named) {
                    try p.emitError(p.token_idx, "named argument (positional args must come before named args)");
                    return error.UnexpectedToken;
                }
                try arg_list.appendExisting(try p.parseExpression());
            }
            if (p.peek(0).tag != .COMMA) break;
            p.next();
        }

        if (!try p.expectToken(.RPAREN)) return error.UnexpectedToken;
        p.next();

        p.ast.get(node_idx).first_child = arg_list.start_index;

        return node_idx;
    }

    /// in case of parse error consumes all tokens till .EOL or .SEMICOLON and returns 0
    pub fn parserDeclOrAssign(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .IDENTIFIER);

        const ident_token_idx = p.token_idx;
        p.next();

        // Check for member access chain on LHS: a.b.c = ...
        if (p.peek(0).tag == .DOT) {
            var lhs = try p.ast.append(.{
                .tag = .ATOM,
                .token_index = ident_token_idx,
            });
            lhs = try p.parseMemberAccessChain(lhs);

            if (!try p.expectOneOf(.{
                .ASSIGN,
                .PLUSASSIGN,
                .MINUSASSIGN,
                .MULTASSIGN,
                .DIVASSIGN,
            }, "assignment operator"))
                return error.UnexpectedToken;

            const op = p.token_idx;
            p.next();

            const rhs = try p.parseExpression();

            if (!try p.expectEndOfStatement())
                return error.UnexpectedToken;
            p.next();

            const node_idx = try p.ast.append(.{
                .tag = .ASSIGNMENT,
                .token_index = op,
                .first_child = lhs,
            });
            p.ast.get(lhs).next_sibling = rhs;
            return node_idx;
        }

        // COLON → declaration (with optional type)
        if (p.peek(0).tag == .COLON) {
            p.next();
            const decl = try p.parseDeclaration(ident_token_idx);
            if (!try p.expectEndOfStatement())
                return error.UnexpectedToken;
            p.next();
            return decl;
        }

        // Assignment operators
        if (!try p.expectOneOf(.{
            .ASSIGN,
            .PLUSASSIGN,
            .MINUSASSIGN,
            .MULTASSIGN,
            .DIVASSIGN,
        }, "assignment or declaration operator"))
            return error.UnexpectedToken;

        const variable_idx = try p.ast.append(.{
            .tag = .ATOM,
            .token_index = ident_token_idx,
        });

        const op = p.token_idx;
        p.next();

        const rhs = try p.parseExpression();

        if (!try p.expectEndOfStatement())
            return error.UnexpectedToken;
        p.next();

        const node_idx = try p.ast.append(.{
            .tag = .ASSIGNMENT,
            .token_index = op,
            .first_child = variable_idx,
        });
        p.ast.get(variable_idx).next_sibling = rhs;
        return node_idx;
    }

    /// Parses declaration after IDENT COLON has been consumed.
    /// Forms:
    ///   x : = expr       → DECLARATION(x), children: [expr]
    ///   x : Type = expr  → DECLARATION(x), children: [TYPE(Type), expr]
    ///   x : Type         → DECLARATION(x), children: [TYPE(Type)]
    fn parseDeclaration(p: *Parser, ident_token_idx: TokenIndex) InternalParserError!AstNodeIndex {
        const node_idx = try p.ast.append(.{
            .tag = .DECLARATION,
            .token_index = ident_token_idx,
        });

        if (p.peek(0).tag == .ASSIGN) {
            // x : = expr (no type)
            p.next();
            const rhs = try p.parseExpression();
            p.ast.get(node_idx).first_child = rhs;
        } else if (p.peek(0).tag == .IDENTIFIER or p.peek(0).tag == .LBRACKET) {
            // x : Type ...
            const type_idx = try p.parseType();
            p.ast.get(node_idx).first_child = type_idx;

            if (p.peek(0).tag == .ASSIGN) {
                // x : Type = expr
                p.next();
                const rhs = try p.parseExpression();
                p.ast.get(type_idx).next_sibling = rhs;
            }
            // else: x : Type (no value) — valid in struct context, checked later
        } else {
            try p.emitError(p.token_idx, "type name or '='");
            return error.UnexpectedToken;
        }

       

        return node_idx;
    }

    pub fn parseWhile(p: *Parser) InternalParserError!AstNodeIndex {
        std.debug.assert(p.peek(0).tag == .WHILE);
        const node_idx = try p.ast.append(.{ .token_index = p.token_idx, .tag = .WHILE });
        p.next();

        const condition_idx = try p.parseExpression();

        if (!try p.expectToken(.EOL))
            return error.UnexpectedToken;
        p.next();

        const body_idx = try p.parseIndentedCodeBlock();

        p.ast.get(node_idx).first_child = condition_idx;
        p.ast.get(condition_idx).next_sibling = body_idx;

        return node_idx;
    }

    pub fn parseIf(p: *Parser) InternalParserError!AstNodeIndex {
        // std.debug.assert(p.peek(0).tag == .IF);
        const node_idx = try p.ast.append(.{ .token_index = p.token_idx, .tag = .IF });
        p.next();

        const condition_idx = try p.parseExpression();

        if (!try p.expectToken(.EOL))
            return error.UnexpectedToken;
        p.next();

        const then_idx = try p.parseIndentedCodeBlock();

        var else_idx: AstNodeIndex = 0;
        if (p.peek(0).tag == .ELSE) {
            p.next();
            if (!try p.expectToken(.EOL))
                return error.UnexpectedToken;
            p.next();

            else_idx = try p.parseIndentedCodeBlock();
        }

        p.ast.get(node_idx).first_child = condition_idx;
        p.ast.get(condition_idx).next_sibling = then_idx;
        if (else_idx > 0)
            p.ast.get(then_idx).next_sibling = else_idx;

        return node_idx;
    }

    pub fn parseStatement(p: *Parser) ParserError!AstNodeIndex {
        // std.debug.print("####parseStatement: {} {s}[{s}]\n", .{ p.token_idx, @tagName(p.peek(0).tag), p.peek(0).str(p.source) });
        var res: InternalParserError!AstNodeIndex = 0;
        switch (p.peek(0).tag) {
            // zig fmt: off
            .IF         => res = p.parseIf(),
            .WHILE      => res = p.parseWhile(),
            .RETURN     => res = p.parseReturn(),
            .FN         => res = p.parseFnDecl(),
            .STRUCT     => res = p.parseStructDecl(),
            // zig fmt: on
            .IDENTIFIER => {
                if (p.peek(1).tag == .LPAREN) {
                    res = p.parseCallOrInstStatement();
                } else {
                    res = p.parserDeclOrAssign();
                }
            },
            else => {
                try p.emitError(p.token_idx, "statement");
                res = error.UnexpectedToken;
            },
        }

        return res catch |err| switch (err) {
            error.UnexpectedToken => {
                p.advanceTillEoStatement();
                p.next();
                return 0;
            },
            else => |e| e,
        };
    }

    pub fn parseIndentedCodeBlock(p: *Parser) ParserError!AstNodeIndex {
        if (!try p.expectToken(.BEGIN_BLOCK))
            return try p.ast.append(.{
                .token_index = p.token_idx,
                .tag = .BLOCK,
            }); // empty block
        p.next();

        const block_idx = try p.parseCodeBlock();

        if (!try p.expectToken(.END_BLOCK))
            return block_idx; // continue parsing despite error
        p.next();

        return block_idx;
    }

    // does not consume .BEGIN_BLOCK and .END_BLOCK!
    pub fn parseCodeBlock(p: *Parser) ParserError!AstNodeIndex {
        assert(p.peek(0).tag != .BEGIN_BLOCK);

        const token_idx = p.token_idx;
        const block_node_idx = try p.ast.append(.{
            .token_index = token_idx,
            .tag = .BLOCK,
        });

        var child_list = p.ast.startList();

        loop: while (true) {
            const token = p.peek(0);
            // std.debug.print("#### {} {s}[{s}]\n", .{ p.token_idx, @tagName(token.tag), token.str(p.source) });
            switch (token.tag) {
                // zig fmt: off
                .END_BLOCK, .EOF => break :loop,
                .EOL, .SEMICOLON  => p.next(), // ignore
                .INVALID_INDENT => { // we should probably handle this in the tokenizer already
                    try p.emitError(p.token_idx, "statement");
                    p.advanceTillEoStatement();
                    p.next();
                    p.next();
                },
                else => {
                    // std.debug.print("token.tag={}\n", .{ @tagName(token.tag) });
                    const node_idx = try p.parseStatement();
                    try child_list.appendExisting(node_idx);
                },
                // zig fmt: on

            } // switch token
        }

        p.ast.get(block_node_idx).first_child = child_list.start_index;
        return block_node_idx;
    }

    // does not consume .BEGIN_BLOCK and .END_BLOCK!
    pub fn parseDeclBlock(p: *Parser) ParserError!AstNodeIndex {
        assert(p.peek(0).tag != .BEGIN_BLOCK);

        const token_idx = p.token_idx;
        const block_node_idx = try p.ast.append(.{
            .token_index = token_idx,
            .tag = .BLOCK,
        });

        var child_list = p.ast.startList();

        loop: while (true) {
            const token = p.peek(0);
            // std.debug.print("#### {} {s}[{s}]\n", .{ p.token_idx, @tagName(token.tag), token.str(p.source) });
            switch (token.tag) {
                // zig fmt: off
                .END_BLOCK, .EOF => break :loop,
                .EOL, .SEMICOLON  => p.next(), // ignore
                .FN => {
                    const node_idx = try p.advanceTillEoStatementOnErr(p.parseFnDecl());
                    try child_list.appendExisting(node_idx);
                },
                .STRUCT => {
                    const node_idx = try p.advanceTillEoStatementOnErr(p.parseStructDecl());
                    try child_list.appendExisting(node_idx);
                },
                else => {
                    // std.debug.print("token.tag={}\n", .{ @tagName(token.tag) });
                    try p.emitError(p.token_idx, "function or struct declaration");
                    p.advanceTillEoStatement();
                },
                // zig fmt: on

            } // switch token
        }

        p.ast.get(block_node_idx).first_child = child_list.start_index;
        return block_node_idx;
    }

    pub fn parse(p: *Parser) ParserError!AstNodeIndex {
        // p.root_node = if (p.peek(0).tag == .BEGIN_BLOCK) try p.parseIndentedCodeBlock() else try p.parseCodeBlock();
        p.root_node = try p.parseDeclBlock();
        return p.root_node;
    }

    pub fn printErrors(p: *const Parser, writer: *std.Io.Writer, ts: TokenStream) !void {
        for (p.errors.items) |err| {
            const pos = try ts.printLineAndMarkToken(writer, err.token_index);
            const token = ts.tokens[err.token_index];
            try writer.print("line {}: Error: found {s}. Expected: {s}\n\n", .{ pos.line, @tagName(token.tag), err.expected });
        }
        try writer.flush();
    }

    pub fn printAstBranch(p: *const Parser, writer: *std.Io.Writer, ast_index: AstNodeIndex, indentation: usize) !void {
        try par.printAstBranch(writer, ast_index, &p.ast, p.tokens, p.source, indentation);
    }

    pub fn printAllNodesFlat(p: *const Parser, writer: *std.Io.Writer) !void {
        for (p.ast.nodes.items, 0..) |node, idx| {
            const token = p.tokens[node.token_index];
            try writer.print("{s} ({s}[{s}]))#{} ", .{
                @tagName(node.tag),
                @tagName(token.tag),
                token.str(p.source),
                idx,
            });
        }
        try writer.writeByte('\n');
    }
};

pub fn getFirstAndLastTokenOfAstBranch(ast_index: AstNodeIndex, ast : *const AST) struct{first:TokenIndex, last:TokenIndex} {
    assert(ast_index != 0);

    const node = ast.get(ast_index);
    var first_token_idx = node.token_index;
    var last_token_idx = node.token_index;

    if (node.hasChild()) {
        var child_list = node.children(ast);
        while (child_list.nextIdx()) |child_idx| {
            const child_res = getFirstAndLastTokenOfAstBranch(child_idx, ast);
            assert(child_res.first != 0);
            assert(child_res.last != 0);
            assert(child_res.first <= child_res.last);

            first_token_idx = @min(first_token_idx, child_res.first);
            last_token_idx  = @max(last_token_idx, child_res.last);
        }
    }

    return .{ .first = first_token_idx, .last = last_token_idx };
}

pub fn printAstBranch(writer: *std.Io.Writer, ast_index: AstNodeIndex, ast : *const AST, tokens : []const Token, source : []const u8, indentation: usize) !void {
    try writer.splatByteAll(' ', 4 * indentation);

    if (ast_index == 0) {
        try writer.writeAll("INVALID\n");
        return;
    }

    const node = ast.get(ast_index).*;
    const token = tokens[node.token_index];

    try writer.print("{s} ({s}[{s}]) #{} (next_sibling={})\n", .{
        @tagName(node.tag),
        @tagName(token.tag),
        if (token.tag == .EOL) "" else token.str(source),
        ast_index,
        ast.get(ast_index).next_sibling,
    });

    if (node.hasChild()) {
        var child_list = node.children(ast);
        while (child_list.nextIdx()) |child_idx|
            try printAstBranch(writer, child_idx, ast, tokens, source, indentation + 1);
    }
}


pub fn debugPrintAstBranch(ast_index: AstNodeIndex, ast : *const AST, tokens : []const Token, source : []const u8) void {
    var buffer: [64]u8 = undefined;
    const bw = std.debug.lockStderrWriter(&buffer);
    defer std.debug.unlockStderrWriter();

    printAstBranch(bw, ast_index, ast, tokens, source, 0) catch {};
}