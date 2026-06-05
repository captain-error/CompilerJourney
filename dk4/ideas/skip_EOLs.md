# Idea: Skip EOL emission inside parens in the tokenizer

## Problem

Multi-line `for (...)` headers need newlines between clauses. Currently the parser
must manually `p.skip(.{.EOL})` to handle them, and semicolons can appear on either
side of the newline, making parsing fragile.

## Proposed solution

In the tokenizer's `next()`, when `paren_nesting_level > 0` and we encounter `\n`:

```zig
'\n' => {
    t.at_line_start = true;
    if (t.paren_nesting_level > 0) {
        t.pos += 1;
        return t.next();  // recurse to get the next real token
    }
    return t.emit(.EOL, 1);
},
```

This is consistent with the existing `paren_nesting_level` check in
`checkIndentation()` (line 153), which already skips all indentation processing
inside parens.

## Affected areas

### TokenStream.init (line 406+)
EOL tracking for `line_infos` would need adjustment — line boundaries that fall
inside parens would be invisible to the token stream. A `line_num` increment
needs to happen inside the `paren_nesting_level > 0` branch.

### Parser call sites that rely on EOL inside parens
- `parseFnDecl` uses `p.skip(.{.EOL})` inside `(...)` — becomes a no-op, fine.
- `parseCallOrInst` skips nothing (EOL-separated args aren't supported yet) —
  would break if someone writes `f(a,\n b)` without commas, but commas are
  required so it's fine.
- `parseFor` — the entire motivation. Clause parsing becomes
  `expectToken(.SEMICOLON)` without EOL-skipping.

### Line reporting
`printLineAndMarkToken` uses `token_lines` → `line_infos` to find source lines.
If EOLs aren't emitted, the `line_infos` entries for lines inside parens would
be missing. This could shift line numbers in error messages for code after the
closing paren, but the line_infos array would be populated from the **next**
EOL that IS emitted (outside parens), so the info gap might not cause issues
in practice.

## Open questions
- Should `[` and `]` also suppress EOLs? They already bump `paren_nesting_level`.
- Recursion depth in `next()`: at most one level per skipped newline, fine.
- `pending_block_ends` at EOF: should still emit END_BLOCK regardless of paren
  nesting.
