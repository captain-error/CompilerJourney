const std = @import("std");
const tok = @import("tokenizer.zig");
const par = @import("parser.zig");
const sem = @import("semantic_analyzer.zig");
const cgen = @import("c_generator.zig");

const TokenStream = tok.TokenStream;
const Parser = par.Parser;
const SemanticValidator = sem.SemanticValidator;
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

    var ts = try TokenStream.init(source, gpa);
    defer ts.deinit(gpa);

    std.debug.print("tokens: {}\n", .{ts.tokens.len});

    var parser = Parser.init(source, ts.tokens, gpa);
    defer parser.deinit();

    _ = parser.parse() catch {};
    std.debug.print("ast nodes: {}\n", .{parser.ast_nodes.items.len});

    if (parser.errors.items.len > 0)
        try parser.printErrors(stdout, ts);

    // for (commands) |ast_index| {
    //     par.printAstBranch(parser, ast_index, 1);
    // }

    // validate shit:
    var validator = SemanticValidator.init(
        gpa,
        ts.source,
        ts.tokens,
        parser.ast_nodes.items,
        parser.top_level_commands.items,
    );
    defer validator.deinit();
    try validator.validate();
    try validator.printErrors(stdout, ts);

    if (parser.errors.items.len > 0 or validator.errors.items.len > 0)
        return;

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
        .ast_nodes = parser.ast_nodes.items,
        .ts = ts,
        .writer = &outfile_writer.interface,
    };
    try cgenerator.generate(parser.top_level_commands.items);
}
