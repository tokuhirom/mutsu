# `callframe` counts double-quoted string interpolation blocks as frames

A `{ … }` closure inside a `"…"` string is its own `Block` call frame in Raku:
`"{callframe(0).code.^name}"` is `Block` and the enclosing routine is
`callframe(1)`. mutsu compiled the closure inline and only counted `for` bodies
as extra frames, so every `callframe(N)` inside an interpolation block resolved
one level too high. Test::Output's default assertion names
(`"… on line {callframe(4).line}"`) ran past the synthetic setting frame and
warned `Use of Nil in string context`.

The compiler now counts a `"…"` closure part (the parser's `DoStmt(Block(…))`)
in `callframe_block_depth`, the same mechanism `for` bodies use (#9765).
Heredoc / `qq` closures still parse to a bare expression — the `S{…} = expr`
replacement is desugared through that path and relies on it — so they are not
counted yet (#9999).
