const std = @import("std");

pub const Token = struct {
    start: u32 = 0,
    end: u32 = 0,
    tag: Tag = .INVALID,

    pub const Tag = enum(u8) {
        INVALID = 0,

        IDENTIFIER,
        NUM,

        STARSTAR,
        STAR,
        SLASH,
        PLUS,
        MINUS,

        LPAREN,
        RPAREN,

        COLONEQ,
        EQ,
        STAREQ,
        SLASHEQ,
        PLUSEQ,
        MINUSEQ,

        EOL,
        SEMICOLON,
        COMMENT,
        EOF,
    };
};

fn U32(v: usize) u32 {
    return @intCast(v);
}

pub const Tokenizer = struct {
    source: []const u8,
    pos: usize = 0,

    fn emit(t: *Tokenizer, tag: Token.Tag, len: usize) Token {
        const token = Token{ .start = U32(t.pos), .end = U32(t.pos + len), .tag = tag };
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

    pub fn next(t: *Tokenizer) Token {

        // chomp whitespace:
        for (t.source[t.pos..]) |char| {
            if (char != ' ' and char != '\r' and char != '\t')
                break;
            t.pos += 1;
        }

        // .EOF
        if (t.pos == t.source.len)
            return t.emit(.EOF, 0);

        const tail = t.source[t.pos..];

        // COMMENT:
        if (tail[0] == '#') {
            for (tail[1..], 1..) |char, len| {
                if (char == '\n')
                    return t.emit(.COMMENT, len);
            }
            return t.emit(.COMMENT, tail.len);
        }

        // .EOL
        if (tail[0] == '\n')
            return t.emit(.EOL, 1);

        // .SEMICOLON
        if (tail[0] == ';')
            return t.emit(.SEMICOLON, 1);

        // .LPAREN
        if (tail[0] == '(')
            return t.emit(.LPAREN, 1);

        // .RPAREN
        if (tail[0] == ')')
            return t.emit(.RPAREN, 1);

        // .EQ
        if (tail[0] == '=')
            return t.emit(.EQ, 1);

        // .PLUS, .PLUSEQ
        if (tail[0] == '+') {
            if (tail.len >= 2 and tail[1] == '=')
                return t.emit(.PLUSEQ, 2);
            return t.emit(.PLUS, 1);
        }

        // .MINUS, .MINUSEQ
        if (tail[0] == '-') {
            if (tail.len >= 2 and tail[1] == '=')
                return t.emit(.MINUSEQ, 2);
            return t.emit(.MINUS, 1);
        }

        // .STAR, .STAREQ
        if (tail[0] == '*') {
            if (tail.len >= 2) {
                if (tail[1] == '=')
                    return t.emit(.STAREQ, 2);
                if (tail[1] == '*')
                    return t.emit(.STARSTAR, 2);
            }
            return t.emit(.STAR, 1);
        }

        // .SLASH, .SLASHEQ
        if (tail[0] == '/') {
            if (tail.len >= 2 and tail[1] == '=')
                return t.emit(.SLASHEQ, 2);
            return t.emit(.SLASH, 1);
        }

        // .COLONEQ
        if (tail.len >= 2 and tail[0] == ':' and tail[1] == '=')
            return t.emit(.COLONEQ, 2);

        // .IDENTIFIER
        if (isvarStart(tail[0])) {
            for (tail[1..], 1..) |char, len| {
                if (!isvarChar(char))
                    return t.emit(.IDENTIFIER, len);
            }
            return t.emit(.IDENTIFIER, tail.len);
        }

        // .NUM
        if (isNumStart(tail[0])) {
            var point_found = tail[0] == '.';
            for (tail[1..], 1..) |char, len| {
                if (char == '.') {
                    // only 1 point allowed
                    if (point_found) return t.emit(.INVALID, len);
                    point_found = true;
                    continue;
                }
                if (isLetter(char)) return t.emit(.INVALID, len);
                if (!isNum(char)) return t.emit(.NUM, len);
            }
            return t.emit(.NUM, tail.len);
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

    pub const TokenIndex = usize;

    pub fn init(source: []const u8, gpa: std.mem.Allocator) !TokenStream {
        var res = TokenStream{ .source = source };

        var tokenizer = Tokenizer{ .source = source };

        try res._tokens.append(gpa, .{});
        try res._token_lines.append(gpa, 0);

        var line_num: u32 = 0;
        var line_info = LineInfo{ .start = 0, .end = undefined };
        while (true) {
            const token = tokenizer.next();
            if (token.tag != .COMMENT) { // ommit comments
                try res._tokens.append(gpa, token);
                try res._token_lines.append(gpa, line_num);
            }

            if (token.tag == .EOL) {
                // info for line which ends here
                line_info.end = token.start;
                try res._line_infos.append(gpa, line_info);

                // info for starting line:
                line_info.start = token.end;
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
        return ts.source[token.start..token.end];
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

    //     std.debug.print(".{s: <10}\"{s}\"\n", .{ @tagName(token.tag), source[token.start..token.end] });
    //     if (token.tag == .EOF) break;
    // }

    const expect = std.testing.expect;
    const expEql = std.testing.expectEqualStrings;

    var tokenizer = Tokenizer{ .source = source };
    var t: Token = undefined;

    // zig fmt: off
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "x"     );
    t = tokenizer.next(); try expect(t.tag == .COLONEQ    ); try expEql(source[t.start..t.end], ":="    );
    t = tokenizer.next(); try expect(t.tag == .NUM        ); try expEql(source[t.start..t.end], "5.0"   );
    t = tokenizer.next(); try expect(t.tag == .COMMENT    ); try expEql(source[t.start..t.end], "# declaring and assigning a variable" );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(source[t.start..t.end], "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "x"     );
    t = tokenizer.next(); try expect(t.tag == .EQ         ); try expEql(source[t.start..t.end], "="     );
    t = tokenizer.next(); try expect(t.tag == .NUM        ); try expEql(source[t.start..t.end], "2.0"   );
    t = tokenizer.next(); try expect(t.tag == .COMMENT    ); try expEql(source[t.start..t.end], "# assigning an existing variable" );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(source[t.start..t.end], "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "y"     );
    t = tokenizer.next(); try expect(t.tag == .COLONEQ    ); try expEql(source[t.start..t.end], ":="    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "x"     );
    t = tokenizer.next(); try expect(t.tag == .PLUS       ); try expEql(source[t.start..t.end], "+"     );
    t = tokenizer.next(); try expect(t.tag == .NUM        ); try expEql(source[t.start..t.end], "3"     );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(source[t.start..t.end], "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "z"     );
    t = tokenizer.next(); try expect(t.tag == .COLONEQ    ); try expEql(source[t.start..t.end], ":="    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "x"     );
    t = tokenizer.next(); try expect(t.tag == .STARSTAR   ); try expEql(source[t.start..t.end], "**"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "y"     );
    t = tokenizer.next(); try expect(t.tag == .SEMICOLON  ); try expEql(source[t.start..t.end], ";"     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "p"     );
    t = tokenizer.next(); try expect(t.tag == .COLONEQ    ); try expEql(source[t.start..t.end], ":="    );
    t = tokenizer.next(); try expect(t.tag == .NUM        ); try expEql(source[t.start..t.end], "7.1"   );
    t = tokenizer.next(); try expect(t.tag == .EOL        ); try expEql(source[t.start..t.end], "\n"    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "result");
    t = tokenizer.next(); try expect(t.tag == .COLONEQ    ); try expEql(source[t.start..t.end], ":="    );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "z"     );
    t = tokenizer.next(); try expect(t.tag == .PLUS       ); try expEql(source[t.start..t.end], "+"     );
    t = tokenizer.next(); try expect(t.tag == .IDENTIFIER ); try expEql(source[t.start..t.end], "p"     );
    t = tokenizer.next(); try expect(t.tag == .EOF        ); try expEql(source[t.start..t.end], ""      );
    // zig fmt: on
}
