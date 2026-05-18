const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const util = @import("util.zig");
const ft_ast = @import("ft_ast.zig");
const nr = @import("name_resolution.zig");
const elab = @import("ast_elaboration.zig");
const type_inference = @import("type_inference.zig");

const assert = std.debug.assert;

const Token = tok.Token;
const TokenStream = tok.TokenStream;
const TokenIndex = TokenStream.TokenIndex;
const SourceLoc = tok.SourceLoc;

// const AST = par.AST;
// const AstNode = par.AstNode;
// const AstNodeIndex = par.AstNodeIndex;

// const FtAst = ft_ast.FtAst;
const DkType = ft_ast.DkType;
// const TypeError = ft_ast.TypeError;
const TypeErrorInfo = ft_ast.TypeErrorInfo;



const infer = type_inference.infer;
const arrayShapeFromLit = type_inference.arrayShapeFromLit;


// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

test "merged: zinseszins program" {
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

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    // Should have 4 fn_decls: [0]=invalid, [1]=do_zins(FLOAT,FLOAT), [2]=zinseszins(FLOAT,FLOAT,INT), [3]=main()
    // do_zins is also called with INT args from the lowered code? No — zinseszins passes result (FLOAT) and zins (FLOAT).
    // Actually: do_zins is called once with (FLOAT, FLOAT). zinseszins is called once with (FLOAT, FLOAT, INT).
    try std.testing.expectEqual(@as(usize, 4), ft.fn_decls.items.len);

    // Each function should have a valid body scope
    for (ft.fn_decls.items[1..]) |fn_decl|
        try std.testing.expect(fn_decl.body_scope != 0);

    // Check return types
    try std.testing.expectEqual(DkType.FLOAT, ft.fn_decls.items[1].return_type); // do_zins returns FLOAT
    try std.testing.expectEqual(DkType.FLOAT, ft.fn_decls.items[2].return_type); // zinseszins returns FLOAT
    try std.testing.expectEqual(DkType.FLOAT, ft.fn_decls.items[3].return_type); // main returns FLOAT
}

test "merged: type error in binary op" {
    const source =
        \\fn main()
        \\    x := 1
        \\    y := true
        \\    result = x + y
    ;

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

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(ft.hasErrors());
    // x + y where x:INT, y:BOOL → "RHS must be of number type"
    try std.testing.expectEqual(@as(usize, 1), ft.errors.items.len);
    try std.testing.expect(ft.errors.items[0].error_ == .wrong_type);
}

test "merged: generic function instantiation" {
    const source =
        \\fn add(a, b)
        \\    result = a + b
        \\
        \\fn main()
        \\    x := add(1, 2)
        \\    y := add(1.0, 2.0)
        \\    result = x + y
    ;

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

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    // add(INT,INT) returns INT, add(FLOAT,FLOAT) returns FLOAT, x + y is type mismatch (INT + FLOAT)
    try std.testing.expect(ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.errors.items.len);
    try std.testing.expect(ft.errors.items[0].error_ == .type_mismatch);
    try std.testing.expectEqual(DkType.INT, ft.errors.items[0].error_.type_mismatch.lhs);
    try std.testing.expectEqual(DkType.FLOAT, ft.errors.items[0].error_.type_mismatch.rhs);

    // Should have 4 fn_decls: invalid + add(INT,INT) + add(FLOAT,FLOAT) + main
    try std.testing.expectEqual(@as(usize, 4), ft.fn_decls.items.len);
}

test "function with default args" {
    const source =
        \\fn greet(a, b := 10)
        \\    result = a + b
        \\
        \\fn main()
        \\    x := greet(1)
        \\    y := greet(2, 3)
        \\    result = x + y
    ;

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

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(!ft.hasErrors());
    // greet(INT,INT) monomorphized once, main
    try std.testing.expectEqual(@as(usize, 3), ft.fn_decls.items.len);
}

test "function with named args" {
    const source =
        \\fn calc(a, b)
        \\    result = a - b
        \\
        \\fn main()
        \\    result = calc(b=1, a=5)
    ;

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

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(!ft.hasErrors());
}

test "function missing required arg" {
    const source =
        \\fn calc(a, b)
        \\    result = a + b
        \\
        \\fn main()
        \\    result = calc(1)
    ;

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

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .missing_required_arg);
}

test "non-default param after default" {
    const source =
        \\fn bad(a := 1, b)
        \\    result = a + b
        \\
        \\fn main()
        \\    result = bad(1, 2)
    ;

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

    try std.testing.expect(di.hasErrors());
    var found = false;
    for (di.errors.items) |e| {
        if (e == .non_default_param_after_default)
            found = true;
    }
    try std.testing.expect(found);
}

test "supply wrong type to fn param with default" {
    const source =
        \\fn bad(a, b:=1)
        \\    result = a + b
        \\
        \\fn main()
        \\    result = bad(1, true)
    ;

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

    var ft = try infer(gpa, ts, &pr.ast, &di);
    defer ft.deinit();

    try std.testing.expect(ft.hasErrors());
    var err : ?TypeErrorInfo = null;
    for (ft.errors.items) |e| {
        if (e.error_ == .wrong_type)
            err = e;
    }

    try std.testing.expect(err != null);
    try std.testing.expect(err.?.error_.wrong_type.actual == DkType.BOOL); // error is on the call site
}

// -----------------------------------------------------------------------
// arrayShapeFromLit unit tests
// -----------------------------------------------------------------------

fn parseExprForShapeTest(source: []const u8, gpa: std.mem.Allocator) !struct { ast: par.AST, root: par.AstNodeIndex } {
    var ts = try tok.TokenStream.init(source, gpa);
    defer ts.deinit(gpa);
    var p = try par.Parser.init(source, ts.tokens, gpa);
    const root = try p.parseExpression();
    return .{ .ast = p.ast, .root = root };
}

test "arrayShapeFromLit: flat [1,2,3]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[1,2,3]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 1), shape.ndim);
    try std.testing.expectEqual(@as(u16, 3), shape.dims[0].count);
    try std.testing.expect(!shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: fill in middle [1,0...,2]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[1,0...,2]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 1), shape.ndim);
    try std.testing.expectEqual(@as(u16, 2), shape.dims[0].count);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: only fill [0...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[0...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 1), shape.ndim);
    try std.testing.expectEqual(@as(u16, 0), shape.dims[0].count);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: 2D [[1,2],[3,4]]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[[1,2],[3,4]]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 2), shape.ndim);
    try std.testing.expectEqual(@as(u16, 2), shape.dims[0].count);
    try std.testing.expect(!shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(u16, 2), shape.dims[1].count);
    try std.testing.expect(!shape.dims[1].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: 2D with fill row [[1,2,3],[0...]...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[[1,2,3],[0...]...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 2), shape.ndim);
    try std.testing.expectEqual(@as(u16, 1), shape.dims[0].count);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expectEqual(@as(u16, 3), shape.dims[1].count);
    try std.testing.expect(!shape.dims[1].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: 2D all fills [[0...]...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[[0...]...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expectEqual(@as(u8, 2), shape.ndim);
    try std.testing.expect(shape.dims[0].has_fill);
    try std.testing.expect(shape.dims[1].has_fill);
    try std.testing.expectEqual(@as(par.AstNodeIndex, 0), shape.extra_fill_node);
}

test "arrayShapeFromLit: multiple fills same level [1,0...,2,0...]" {
    const gpa = std.testing.allocator;
    var result = try parseExprForShapeTest("[1,0...,2,0...]", gpa);
    defer result.ast.deinit();
    const shape = arrayShapeFromLit(&result.ast, result.root);
    try std.testing.expect(shape.extra_fill_node != 0);
}

// -----------------------------------------------------------------------
// Full pipeline array type inference tests
// -----------------------------------------------------------------------

fn inferProgram(source: []const u8, gpa: std.mem.Allocator) !ft_ast.FtAst {
    var ts = try tok.TokenStream.init(source, gpa);
    defer ts.deinit(gpa);
    var pr = try par.parse(source, ts.tokens, gpa);
    defer pr.deinit();
    var di = try nr.resolve(gpa, &pr.ast, ts.tokens, source, pr.root_node);
    defer di.deinit();
    return infer(gpa, ts, &pr.ast, &di);
}

test "array: annotated [3]Int = [1,2,3]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,2,3]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.array_instances.items.len);
    const ai = ft.array_instances.items[0];
    try std.testing.expectEqual(@as(u8, 1), ai.ndim);
    try std.testing.expectEqual(@as(u16, 3), ai.shape[0]);
    try std.testing.expectEqual(DkType.INT, ai.elem_type);
    // var decl should have array type
    const main_scope = ft.fn_decls.items[1].body_scope;
    const var_decl_idx = ft.scopes.items[main_scope].first_decl;
    try std.testing.expect(ft.var_decls.items[var_decl_idx].type_.isArray());
}

test "array: inferred := [1,2,3]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a := [1,2,3]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.array_instances.items.len);
    const ai = ft.array_instances.items[0];
    try std.testing.expectEqual(@as(u8, 1), ai.ndim);
    try std.testing.expectEqual(@as(u16, 3), ai.shape[0]);
    try std.testing.expectEqual(DkType.INT, ai.elem_type);
}

test "array: 2D annotated [2,3]Int" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [2,3]Int = [[1,2,3],[4,5,6]]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    // expect 2 array instances: [3]Int (inner) and [2,3]Int (outer)
    var found_outer = false;
    for (ft.array_instances.items) |ai| {
        if (ai.ndim == 2 and ai.shape[0] == 2 and ai.shape[1] == 3 and ai.elem_type == DkType.INT)
            found_outer = true;
    }
    try std.testing.expect(found_outer);
}

test "array: fill with annotation [4]Int = [1,0...,2]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [4]Int = [1,0...,2]", gpa);
    defer ft.deinit();
    try std.testing.expect(!ft.hasErrors());
    try std.testing.expectEqual(@as(usize, 1), ft.array_instances.items.len);
    try std.testing.expectEqual(@as(u16, 4), ft.array_instances.items[0].shape[0]);
}

test "array: shape mismatch [3]Int = [1,2]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,2]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .array_shape_mismatch);
}

test "array: cannot infer size with fill" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a := [1,0...]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .cannot_infer_array_size);
}

test "array: multiple fills same level" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,0...,2,0...]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    var found = false;
    for (ft.errors.items) |e| if (e.error_ == .multiple_fills_in_array_literal) { found = true; break; };
    try std.testing.expect(found);
}

test "array: inconsistent elem types [1,2,true]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [1,2,true]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    var found = false;
    for (ft.errors.items) |e| if (e.error_ == .inconsistent_elem_types_in_array_lit) { found = true; break; };
    try std.testing.expect(found);
}

test "array: ndim mismatch [3]Int = [[1,2,3]]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [3]Int = [[1,2,3]]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    try std.testing.expect(ft.errors.items[0].error_ == .array_shape_mismatch);
}

test "array: inconsistent inner dim [[1,2],[1,2,3]]" {
    const gpa = std.testing.allocator;
    var ft = try inferProgram("fn main()\n    a : [2,3]Int = [[1,2],[1,2,3]]", gpa);
    defer ft.deinit();
    try std.testing.expect(ft.hasErrors());
    var found = false;
    for (ft.errors.items) |e| if (e.error_ == .inconsistent_inner_dim) { found = true; break; };
    try std.testing.expect(found);
}
