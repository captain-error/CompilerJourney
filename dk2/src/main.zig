const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const typing = @import("type_inference.zig");
const cgen = @import("c_generator.zig");

const TokenStream = tok.TokenStream;
const Parser = par.Parser;
const TypeInferer = typing.TypeInferer;
const CGenerator = cgen.CGenerator;

fn readSource(path: []const u8, allocator: std.mem.Allocator) ![]const u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Failed to open source file: {s}\n", .{@errorName(err)});
        return err;
    };
    defer file.close();

    const filesize = try file.getEndPos();
    const buff = try allocator.alloc(u8, filesize);
    const num_byte_read = try file.readAll(buff);

    if (num_byte_read != filesize)
        return error.CouldNotReadCompleteSourceFromFile;

    return buff;
}

fn outputPathFromInputPath(inpath: []const u8, buff: []u8) ![]const u8 {
    var index: usize = inpath.len;
    for (1..inpath.len) |i| {
        if (inpath[inpath.len - i] == '.') {
            index = inpath.len - i;
            break;
        }
    }

    const outlen = index + 2;

    if (buff.len < outlen)
        return error.OutPathTooLong;

    @memcpy(buff.ptr, inpath[0..index]);
    buff.ptr[index + 0] = '.';
    buff.ptr[index + 1] = 'c';
    return buff.ptr[0..outlen];
}

pub fn main() !void {
    var gpa_ = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len != 2) {
        std.log.err("Usage: {s} <source.dk1>\n", .{args[0]});
        return;
    }

    const source_path = args[1];
    var outpath_buff: [512]u8 = undefined;
    const outpath = try outputPathFromInputPath(source_path, &outpath_buff);

    const source = try readSource(source_path, gpa);
    defer gpa.free(source);

    // const source =
    //     \\ x := -5.0 # declaring and assigning a variable
    //     \\ x = 2.0  # assigning an existing variable
    //     \\ d := 1
    //     \\ y := -x + -3 * - (-7 + -2) **-x + d
    //     \\ z := x ** y; p := 7.1
    //     \\ result := z + p**2
    // ;

    var stdout_buff: [512]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buff);
    const stdout = &stdout_writer.interface;
    defer stdout.flush() catch {};

    // -----------------------------------------

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    // try ts.prettyPrintTokens(stdout);

    // std.debug.print("tokens: {}\n", .{ts.tokens.len});

    var parser = try Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();

    _ = try parser.parse();
    // std.debug.print("ast nodes: {}\n", .{parser.ast.nodeCount()});
    try parser.printAstBranch(stdout, parser.root_node, 1);
    try stdout.flush();

    if (parser.hasErrors()) {
        try stdout.writeAll("There were errors in the parsing stage:\n");
        try parser.printErrors(stdout, ts);
        return;
    }

    // for (commands) |ast_index| {
    //     par.printAstBranch(parser, ast_index, 1);
    // }

    var ti = try TypeInferer.init(gpa, &parser);
    defer ti.deinit();

    try ti.checkAndReconstructTypes( parser.root_node);
    
    if(ti.hasErrors()) {
        try stdout.writeAll("There were errors in the semantic analysis stage:\n");
        try ti.printErrors(stdout, ts);
        return;
    }
    try typing.printAstBranchWithTypes(&parser, stdout, ti.node_types, parser.root_node, 1);
    try stdout.flush();

    // open outfile:
    const outfile = std.fs.cwd().createFile(outpath, .{}) catch |err| {
        std.log.err("Failed to open output file \"{s}\"\n\t{s}\n", .{ outpath, @errorName(err) });
        return;
    };
    defer outfile.close();
    var outbuff: [2048]u8 = undefined;
    var outfile_writer = outfile.writer(&outbuff);

    // generate C code
    var cgenerator = CGenerator{
        .ast = parser.ast, // shallow copy
        .node_types = ti.node_types,
        .ts = ts,
        .writer = &outfile_writer.interface,
    };
    try cgenerator.generate(parser.root_node);
}
