const std = @import("std");

pub const Token = struct {
    start: u32 = 0,
    len: u16 = 0,
    tag: Tag = .INVALID,

    pub fn str(t: Token, source: []const u8) []const u8 {
        return source[t.start..t.end()];
    }

    pub fn end(t: Token) u32 {
        return t.start + t.len;
    }

    pub const Tag = enum(u8) {
        INVALID = 0,
        INVALID_INDENT,

        IDENTIFIER,
        FLOAT_LIT,
        INT_LIT,
        TRUE,
        FALSE,

        POW,
        TIMES,
        DIV,
        PLUS,
        MINUS,

        GT,
        LT,
        GE,
        LE,
        EQ,
        NOT_EQ,

        BW_NOT,
        BW_AND,
        BW_XOR,
        BW_OR,

        AND,
        OR,
        XOR,
        NOT,

        LPAREN,
        RPAREN,
        LBRACKET,
        RBRACKET,

        COLON,
        DOT,
        ELLIPSIS,
        ASSIGN,
        MULTASSIGN,
        DIVASSIGN,
        PLUSASSIGN,
        MINUSASSIGN,

        UNDERSCORE,

        IF,
        ELSE,
        WHILE,
        FN,
        STRUCT,
        RETURN,

        COMMA,
        SEMICOLON,
        COMMENT,

        BEGIN_BLOCK,
        END_BLOCK,

        EOL,
        EOF,
    };
};

fn U16(v: usize) u16 {
    return @intCast(v);
}

fn U32(v: usize) u32 {
    return @intCast(v);
}

pub const MAX_INDENTATION_LEVELS: u8 = 32;

pub const Tokenizer = struct {
    source: []const u8,
    pos: usize = 0,

    // stuff to keep track of indentation:
    at_line_start: bool = true,
    paren_nesting_level: i16 = 0, // indentation is ignored inside parentheses
    indent_stack: [MAX_INDENTATION_LEVELS]u16 = [_]u16{0} ** MAX_INDENTATION_LEVELS,
    indent_depth: u8 = 0,
    pending_block_ends: u8 = 0,

    fn emit(t: *Tokenizer, tag: Token.Tag, len: usize) Token {
        const token = Token{ .start = U32(t.pos), .len = U16(len), .tag = tag };
        t.pos += len;
        return token;
    }

    fn isLetter(char: u8) bool {
        if (char >= 'a' and char <= 'z') return true;
        if (char >= 'A' and char <= 'Z') return true;
        return false;
    }

    fn isNum(char: u8) bool {
        return char >= '0' and char <= '9';
    }

    fn isvarStart(char: u8) bool {
        return isLetter(char) or char == '_';
    }

    fn isvarChar(char: u8) bool {
        return isLetter(char) or char == '_' or isNum(char);
    }

    fn isNumStart(char: u8) bool {
        return isNum(char) or char == '.';
    }

    fn checkIndentation(t: *Tokenizer) ?Token {
        // Emit any queued BLOCK_END tokens before reading new input.
        if (t.pending_block_ends > 0) {
            t.pending_block_ends -= 1;
            return t.emit(.END_BLOCK, 0);
        }

        // Note: For empty/comment-only lines the leading spaces are left for the
        //       whitespace chomper so normal token emission still works.

        // At the start of a new line, determine the indentation level.
        if (!t.at_line_start)
            return null;

        t.at_line_start = false;

        if (t.paren_nesting_level != 0)
            return null; // inside parentheses: no indentation to check

        // Count leading spaces; tabs are not allowed.
        var space_count: usize = 0;
        for (t.source[t.pos..]) |char| {
            if (char == '\t') return t.emit(.INVALID_INDENT, space_count + 1); // FIXME: better error messaging!
            if (char != ' ') break;
            space_count += 1;
        }

        // Check whether this line contains real code (not empty / comment-only).
        const peek_pos = t.pos + space_count;
        const is_real_code = peek_pos < t.source.len and
            t.source[peek_pos] != '\n' and
            t.source[peek_pos] != '\r' and
            t.source[peek_pos] != '#';

        if (is_real_code) {
            const new_indent: u16 = @intCast(space_count);
            const cur_indent = t.indent_stack[t.indent_depth];

            if (new_indent > cur_indent) {
                // Indentation increased: push the new level and emit BEGIN_BLOCK.
                std.debug.assert(t.indent_depth + 1 < MAX_INDENTATION_LEVELS); // FIXME!
                if (t.indent_depth + 1 >= MAX_INDENTATION_LEVELS)
                    return t.emit(.INVALID_INDENT, space_count); // we somehow need better error messaging here
                t.indent_depth += 1;
                t.indent_stack[t.indent_depth] = new_indent;

                return t.emit(.BEGIN_BLOCK, space_count);
            } else if (new_indent < cur_indent) {
                // Indentation decreased: pop levels and emit END_BLOCKs.
                while (t.indent_depth > 0 and t.indent_stack[t.indent_depth] > new_indent) {
                    t.indent_depth -= 1;
                    t.pending_block_ends += 1;
                }
                if (t.indent_stack[t.indent_depth] != new_indent) {
                    // New indent doesn't match any previous level: error.
                    return t.emit(.INVALID_INDENT, space_count);
                }
                std.debug.assert(t.pending_block_ends > 0);
                t.pending_block_ends -= 1;

                t.pos += space_count; // consume the indentation spaces for the next token
                return t.emit(.END_BLOCK, 0);
            }
        }

        return null;
    }

    pub fn next(t: *Tokenizer) Token {
        if (t.checkIndentation()) |indent_token|
            return indent_token;

        // chomp whitespace:
        for (t.source[t.pos..]) |char| {
            if (char != ' ' and char != '\r' and char != '\t')
                break;
            t.pos += 1;
        }

        // .EOF
        if (t.pos == t.source.len) {
            if (t.indent_depth > 0) {
                // close any remaining open indentation blocks first.
                t.pending_block_ends = t.indent_depth;
                t.indent_depth = 0;
                return t.emit(.EOL, 0);
            }
            return t.emit(.EOF, 0);
        }

        const tail = t.source[t.pos..];

        switch (tail[0]) {
            '#' => {
                for (tail[1..], 1..) |char, len| {
                    if (char == '\n')
                        return t.emit(.COMMENT, len);
                }
                return t.emit(.COMMENT, tail.len);
            },
            '\n' => {
                t.at_line_start = true;
                return t.emit(.EOL, 1);
            },
            ';' => return t.emit(.SEMICOLON, 1),
            ',' => return t.emit(.COMMA, 1),
            '(' => {
                t.paren_nesting_level += 1;
                return t.emit(.LPAREN, 1);
            },
            ')' => {
                t.paren_nesting_level -= 1;
                return t.emit(.RPAREN, 1);
            },
            '[' => {
                t.paren_nesting_level += 1;
                return t.emit(.LBRACKET, 1);
            },
            ']' => {
                t.paren_nesting_level -= 1;
                return t.emit(.RBRACKET, 1);
            },

            // .GE, .GT
            '>' => {
                if (tail.len >= 2 and tail[1] == '=')
                    return t.emit(.GE, 2);
                return t.emit(.GT, 1);
            },

            // .LE, .LT
            '<' => {
                if (tail.len >= 2 and tail[1] == '=')
                    return t.emit(.LE, 2);
                return t.emit(.LT, 1);
            },

            // .EQ, .ASSIGN
            '=' => {
                if (tail.len >= 2 and tail[1] == '=')
                    return t.emit(.EQ, 2);
                return t.emit(.ASSIGN, 1);
            },

            // .NOT_EQ
            '!' => if (tail[1] == '=') return t.emit(.NOT_EQ, 2),

            // .PLUS, .PLUSASSIGN
            '+' => {
                if (tail.len >= 2 and tail[1] == '=')
                    return t.emit(.PLUSASSIGN, 2);
                return t.emit(.PLUS, 1);
            },

            // .MINUS, .MINUSASSIGN
            '-' => {
                if (tail.len >= 2 and tail[1] == '=')
                    return t.emit(.MINUSASSIGN, 2);
                return t.emit(.MINUS, 1);
            },

            // .TIMES, .MULTASSIGN
            '*' => {
                if (tail.len >= 2) {
                    if (tail[1] == '=')
                        return t.emit(.MULTASSIGN, 2);
                    if (tail[1] == '*')
                        return t.emit(.POW, 2);
                }
                return t.emit(.TIMES, 1);
            },

            // .DIV, .DIVASSIGN
            '/' => {
                if (tail.len >= 2 and tail[1] == '=')
                    return t.emit(.DIVASSIGN, 2);
                return t.emit(.DIV, 1);
            },

            ':' => return t.emit(.COLON, 1),
            '.' => {
                if (tail.len >= 3 and tail[1] == '.' and tail[2] == '.')
                    return t.emit(.ELLIPSIS, 3);
                if (tail.len >= 2 and isNum(tail[1])) {} // fall through to number parsing below
                else return t.emit(.DOT, 1);
            },
            else => {},
        } // switch --------------------------------------

        // .IDENTIFIER etc
        if (isvarStart(tail[0])) {
            var len: usize = 1;
            for (tail[1..]) |char| {
                if (!isvarChar(char))
                    break;
                len += 1;
            }

            const word = tail[0..len];
            // FIXME: this can be done without revisiting
            // zig fmt: off
            if (std.mem.eql(u8, "_"     , word)) return t.emit(.UNDERSCORE, len);
            if (std.mem.eql(u8, "or"    , word)) return t.emit(.OR        , len);
            if (std.mem.eql(u8, "if"    , word)) return t.emit(.IF        , len);
            if (std.mem.eql(u8, "fn"    , word)) return t.emit(.FN        , len);
            if (std.mem.eql(u8, "and"   , word)) return t.emit(.AND       , len);
            if (std.mem.eql(u8, "not"   , word)) return t.emit(.NOT       , len);
            if (std.mem.eql(u8, "xor"   , word)) return t.emit(.XOR       , len);
            if (std.mem.eql(u8, "else"  , word)) return t.emit(.ELSE      , len);
            if (std.mem.eql(u8, "true"  , word)) return t.emit(.TRUE      , len);
            if (std.mem.eql(u8, "false" , word)) return t.emit(.FALSE     , len);
            if (std.mem.eql(u8, "while" , word)) return t.emit(.WHILE     , len);
            if (std.mem.eql(u8, "struct", word)) return t.emit(.STRUCT    , len);
            if (std.mem.eql(u8, "return", word)) return t.emit(.RETURN    , len);
                                                         return t.emit(.IDENTIFIER, len);
            // zig fmt: on

        }

        // .INT_LIT, FLOAT_LIT
        if (isNumStart(tail[0])) {
            var point_found = tail[0] == '.';
            for (tail[1..], 1..) |char, len| {
                if (char == '.') {
                    // only 1 point allowed
                    if (point_found) return t.emit(.INVALID, len);
                    // don't consume a '.' that's the start of '...' (ellipsis)
                    if (t.pos + len + 1 < t.source.len and t.source[t.pos + len + 1] == '.')
                        return t.emit(.INT_LIT, len);
                    point_found = true;
                    continue;
                }
                if (isLetter(char) or char == '_') return t.emit(.INVALID, len);
                if (!isNum(char))
                    return t.emit(if (point_found) .FLOAT_LIT else .INT_LIT, len);
            }

            return t.emit(if (point_found) .FLOAT_LIT else .INT_LIT, tail.len);
        }

        return t.emit(.INVALID, 1);
    } // fn next
};

pub const LineInfo = struct {
    start: u32,
    end: u32 = undefined,
};

pub const SourceLoc = struct { line: u32, column: u32 };

pub const TokenStream = struct {
    tokens: []const Token = undefined,
    token_lines: []const u32 = undefined,
    line_infos: []const LineInfo = undefined,
    source: []const u8,

    _tokens: std.ArrayList(Token) = .empty,
    _token_lines: std.ArrayList(u32) = .empty,
    _line_infos: std.ArrayList(LineInfo) = .empty,

    pub const TokenIndex = u32;

    pub fn init(source: []const u8, gpa: std.mem.Allocator) !TokenStream {
        var res = TokenStream{ .source = source };

        var tokenizer = Tokenizer{ .source = source };

        try res._tokens.append(gpa, .{});
        try res._token_lines.append(gpa, 0);

        var line_num: u32 = 0;
        var line_info = LineInfo{ .start = 0, .end = undefined };
        while (true) {
            const token = tokenizer.next();
            if (token.tag != .COMMENT) { // omit comments
                try res._tokens.append(gpa, token);
                try res._token_lines.append(gpa, line_num);
            }

            if (token.tag == .EOL) {
                // info for line which ends here
                line_info.end = token.start;
                try res._line_infos.append(gpa, line_info);

                // info for starting line:
                line_info.start = token.end();
                line_num += 1;
            }

            if (token.tag == .EOF) {
                line_info.end = token.start;
                try res._line_infos.append(gpa, line_info);
                break;
            }
        }

        std.debug.assert(res._tokens.items.len == res._token_lines.items.len);

        // zig fmt: off
        res.tokens      = res._tokens     .items;      
        res.token_lines = res._token_lines.items;
        res.line_infos  = res._line_infos .items;
        // zig fmt: on

        return res;
    }

    pub fn deinit(ts: *TokenStream, gpa: std.mem.Allocator) void {
        ts._line_infos.deinit(gpa);
        ts._token_lines.deinit(gpa);
        ts._tokens.deinit(gpa);
    }

    pub fn sourceStr(ts: *const TokenStream, token_index: TokenIndex) []const u8 {
        const token = ts.tokens[token_index];
        return token.str(ts.source);
    }

    pub fn sourceLoc(ts: *const TokenStream, token_index: TokenIndex) SourceLoc {
        const token = ts.tokens[token_index];
        const token_line = ts.token_lines[token_index];
        const line_info = ts.line_infos[token_line];
        const column = token.start - line_info.start;
        return .{ .line = token_line, .column = column };
    }

    pub fn printLineAndMarkToken(
        ts: *const TokenStream,
        writer: *std.Io.Writer,
        token_index: TokenIndex,
    ) !SourceLoc {
        const token = ts.tokens[token_index];
        const token_line = ts.token_lines[token_index];
        const line_info = ts.line_infos[token_line];
        const column = token.start - line_info.start;

        try writer.print("{s}\n", .{ts.source[line_info.start..line_info.end]});
        try writer.splatByteAll(' ', column);
        try writer.writeAll("^\n");

        return .{ .line = token_line, .column = column };
    }

    pub fn prettyPrintTokens(ts: *const TokenStream, w: *std.Io.Writer) !void {
        var tokens_on_line: usize = 0;
        for (ts.tokens[1..], 1..) |token, tok_i| {
            if (token.tag == .EOL) {
                try w.writeAll("EOL\n");
                const line = ts.line_infos[ts.token_lines[tok_i]];
                if (tokens_on_line > 0)
                    try w.print("{s}\n\n", .{ts.source[line.start..line.end]});
                tokens_on_line = 0;
            } else {
                try w.print("{s}[{s}] ", .{ @tagName(token.tag), token.str(ts.source) });
                tokens_on_line += 1;
            }
        }
        try w.writeByte('\n');
        try w.flush();
    }
};

test "Tokenizer" {
    const source =
        \\ x := 5.0 # declaring and assigning a variable
        \\ x = 2.0  # assigning an existing variable
        \\ y := x + 3
        \\ z := x ** y; p := 7.1
        \\ result := z + p
    ;
    // var tokenizer = Tokenizer{ .source = source };
    // while (true) {
    //     const token = tokenizer.next();
    //     if (token.tag == .EOL) {
    //         std.debug.print(".EOL\n", .{});
    //         continue;
    //     }

    //     std.debug.print(".{s: <10}\"{s}\"\n", .{ @tagName(token.tag), token.str(source) });
    //     if (token.tag == .EOF) break;
    // }

    const expect = std.testing.expect;
    const expEql = std.testing.expectEqualStrings;

    var tokenizer = Tokenizer{ .source = source };
    var t: Token = undefined;

    // zig fmt: off
    t = tokenizer.next(); try expect(t.tag == .BEGIN_BLOCK); try expEql(t.str(source), " "     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "x"     );
    t = tokenizer.next(); try expect(t.tag == .COLON      ); try expEql(t.str(source), ":"     );
    t = tokenizer.next(); try expect(t.tag == .ASSIGN     ); try expEql(t.str(source), "="     );
    t = tokenizer.next(); try expect(t.tag == .FLOAT_LIT  ); try expEql(t.str(source), "5.0"   );
    t = tokenizer.next(); try expect(t.tag == .COMMENT    ); try expEql(t.str(source), "# declaring and assigning a variable" );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(t.str(source), "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "x"     );
    t = tokenizer.next(); try expect(t.tag == .ASSIGN     ); try expEql(t.str(source), "="     );
    t = tokenizer.next(); try expect(t.tag == .FLOAT_LIT  ); try expEql(t.str(source), "2.0"   );
    t = tokenizer.next(); try expect(t.tag == .COMMENT    ); try expEql(t.str(source), "# assigning an existing variable" );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(t.str(source), "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "y"     );
    t = tokenizer.next(); try expect(t.tag == .COLON      ); try expEql(t.str(source), ":"     );
    t = tokenizer.next(); try expect(t.tag == .ASSIGN     ); try expEql(t.str(source), "="     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "x"     );
    t = tokenizer.next(); try expect(t.tag == .PLUS       ); try expEql(t.str(source), "+"     );
    t = tokenizer.next(); try expect(t.tag == .INT_LIT    ); try expEql(t.str(source), "3"     );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(t.str(source), "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "z"     );
    t = tokenizer.next(); try expect(t.tag == .COLON      ); try expEql(t.str(source), ":"     );
    t = tokenizer.next(); try expect(t.tag == .ASSIGN     ); try expEql(t.str(source), "="     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "x"     );
    t = tokenizer.next(); try expect(t.tag == .POW        ); try expEql(t.str(source), "**"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "y"     );
    t = tokenizer.next(); try expect(t.tag == .SEMICOLON  ); try expEql(t.str(source), ";"     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "p"     );
    t = tokenizer.next(); try expect(t.tag == .COLON      ); try expEql(t.str(source), ":"     );
    t = tokenizer.next(); try expect(t.tag == .ASSIGN     ); try expEql(t.str(source), "="     );
    t = tokenizer.next(); try expect(t.tag == .FLOAT_LIT  ); try expEql(t.str(source), "7.1"   );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(t.str(source), "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "result");
    t = tokenizer.next(); try expect(t.tag == .COLON      ); try expEql(t.str(source), ":"     );
    t = tokenizer.next(); try expect(t.tag == .ASSIGN     ); try expEql(t.str(source), "="     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "z"     );
    t = tokenizer.next(); try expect(t.tag == .PLUS       ); try expEql(t.str(source), "+"     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(t.str(source), "p"     );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(t.str(source), ""     );
    t = tokenizer.next(); try expect(t.tag == .END_BLOCK  ); try expEql(t.str(source), ""      );
    t = tokenizer.next(); try expect(t.tag == .EOF        ); try expEql(t.str(source), ""      );
    // zig fmt: on
}

test "DOT and member access" {
    const source = "s.velocity";
    var tokenizer = Tokenizer{ .source = source };
    const expect = std.testing.expect;
    const expEql = std.testing.expectEqualStrings;
    // zig fmt: off
    var t: Token = undefined;
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER); try expEql(t.str(source), "s");
    t = tokenizer.next(); try expect(t.tag == .DOT       ); try expEql(t.str(source), ".");
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER); try expEql(t.str(source), "velocity");
    t = tokenizer.next(); try expect(t.tag == .EOF       );
    // zig fmt: on
}

test "DOT vs float starting with dot" {
    const source = ".5";
    var tokenizer = Tokenizer{ .source = source };
    const t = tokenizer.next();
    try std.testing.expect(t.tag == .FLOAT_LIT);
    try std.testing.expectEqualStrings(t.str(source), ".5");
}

test "STRUCT keyword" {
    const source = "struct Car";
    var tokenizer = Tokenizer{ .source = source };
    const expect = std.testing.expect;
    const expEql = std.testing.expectEqualStrings;
    var t: Token = undefined;
    t = tokenizer.next();
    try expect(t.tag == .STRUCT);
    try expEql(t.str(source), "struct");
    t = tokenizer.next();
    try expect(t.tag == .IDENTIFIER);
    try expEql(t.str(source), "Car");
    t = tokenizer.next();
    try expect(t.tag == .EOF);
}

test "type annotation" {
    const source = "x : Int = 3";
    var tokenizer = Tokenizer{ .source = source };
    const expect = std.testing.expect;
    const expEql = std.testing.expectEqualStrings;
    var t: Token = undefined;
    // zig fmt: off
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER); try expEql(t.str(source), "x");
    t = tokenizer.next(); try expect(t.tag == .COLON     ); try expEql(t.str(source), ":");
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER); try expEql(t.str(source), "Int");
    t = tokenizer.next(); try expect(t.tag == .ASSIGN    ); try expEql(t.str(source), "=");
    t = tokenizer.next(); try expect(t.tag == .INT_LIT   ); try expEql(t.str(source), "3");
    t = tokenizer.next(); try expect(t.tag == .EOF       );
    // zig fmt: on
}

test "COLON ASSIGN without space" {
    const source = "x:=3";
    var tokenizer = Tokenizer{ .source = source };
    const expect = std.testing.expect;
    var t: Token = undefined;
    t = tokenizer.next();
    try expect(t.tag == .IDENTIFIER);
    t = tokenizer.next();
    try expect(t.tag == .COLON);
    t = tokenizer.next();
    try expect(t.tag == .ASSIGN);
    t = tokenizer.next();
    try expect(t.tag == .INT_LIT);
    t = tokenizer.next();
    try expect(t.tag == .EOF);
}
