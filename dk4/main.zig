const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const elab = @import("ast_elaboration.zig");
const nr = @import("name_resolution.zig");
const type_inference = @import("type_inference.zig");
const c_codegen = @import("c_codegen.zig");

const TokenStream = tok.TokenStream;
const CCodegen = c_codegen.CCodegen;

pub fn main(init: std.process.Init.Minimal) !void {
    const gpa = std.heap.page_allocator;

    // --- Set up I/O ---
    var threaded: std.Io.Threaded = .init(gpa, .{});
    defer threaded.deinit();
    const io = threaded.io();

    // --- Output writers ---
    var stdout_buf: [1024]u8 = undefined;
    var stderr_buf: [1024]u8 = undefined;

    var stdout_file = std.Io.File.stdout();
    var stderr_file = std.Io.File.stderr();

    var stdout_w = stdout_file.writer(io, &stdout_buf);
    var stderr_w = stderr_file.writer(io, &stderr_buf);

    const stdout = &stdout_w.interface;
    const stderr = &stderr_w.interface;
    errdefer {
        stdout.flush() catch {};
        stderr.flush() catch {};
    }

    // --- Read source file ---
    const args = try std.process.Args.toSlice(init.args, gpa);
    defer gpa.free(args);

    if (args.len < 2) {
        try stderr.print("usage: {s} <source.dk4>\n", .{if (args.len > 0) args[0] else "dk4"});
        try stderr.flush();
        std.process.exit(1);
    }

    const source_path = args[1];
    const source = try std.Io.Dir.cwd().readFileAlloc(io, source_path, gpa, .limited(std.math.maxInt(usize)));
    defer gpa.free(source);

    // --- Tokenize ---
    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    // --- Parse ---
    var parse_res = try par.parse(source, ts.tokens, gpa);
    defer parse_res.deinit();

    if (parse_res.hasErrors()) {
        try parse_res.printErrors(stderr, ts);
        try stderr.flush();
        std.process.exit(1);
    }

    // --- Elaborate ---
    var elab_errors = try elab.elaborate(&parse_res.ast, ts.tokens, parse_res.root_node, gpa);
    defer elab_errors.deinit(gpa);

    if (elab_errors.items.len > 0) {
        for (elab_errors.items) |err| {
            try err.print(&parse_res.ast, ts, stderr);
        }
        try stderr.flush();
        std.process.exit(1);
    }

    // --- Name resolution ---
    var di = try nr.resolve(gpa, &parse_res.ast, &ts, source, parse_res.root_node);
    defer di.deinit();

    if (di.hasErrors()) {
        try di.printErrors(&parse_res.ast, ts, stderr);
        try stderr.flush();
        std.process.exit(1);
    }

    // --- Type inference ---
    var infer_res = try type_inference.infer(gpa, ts, &parse_res.ast, &di);
    defer infer_res.deinit(gpa);

    if (infer_res.hasErrors()) {
        try infer_res.printErrors(&parse_res.ast, ts, stderr);
        try stderr.flush();
        std.process.exit(1);
    }

    // --- Code generation ---
    try CCodegen.generate(stdout, &infer_res.ft_ast, gpa);

    try stdout.flush();
}
