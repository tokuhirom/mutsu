# RakuAST origins of heredoc segments and adverbed quotes

`.AST` now places, as rakudo 2026.11 does:

- the segments of an interpolating heredoc (`qq:to/END/` with `$n` inside).
  A literal's range covers the indentation the terminator line strips, so
  `"  a "` written before `$n` is the `StrLiteral` `"a "`;
- the unit's `StatementList` when its last statement opens a heredoc. The
  list ends after the statement's `;`, before the body and its terminator,
  which no longer count as text no statement accounts for;
- the segments of a quote made interpolating by adverbs, such as
  `Q:s|a $n|` or `q:a{x @a[0]}` (#15780).
