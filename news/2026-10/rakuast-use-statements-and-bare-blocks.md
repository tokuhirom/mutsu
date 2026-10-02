# RakuAST: `use` statements cross the boundary, and a lowered bare block runs

The first measurement for ADR-10723 (RakuAST as the frontend IR) ran
`EVAL(slurp($file).AST)` over every `t/` test file. Exactly one of 5638 files
survived: `use Test;` was a construct `.AST` refused, and nearly every test file
starts with it.

`use` statements now convert to the three node kinds rakudo 2026.09 produces and
lower back to the parser's `Stmt::Use` / `Stmt::No`:

- `RakuAST::Pragma` for a core pragma, now including one with an argument
  (`use lib "lib"`) and the `no` form (`off => True`);
- `RakuAST::Statement::LanguageVersion` for `use v6.d`, whose version leaf
  renders as `v6.d`;
- `RakuAST::Statement::Use` for every other module (`experimental` and
  `newline` included), with import tags as one `ColonPair::True` each, or a
  comma list of them.

A statement-level call the parser resolved against an imported routine
(`ok 1, "a"` once `Test` is loaded) is a `Stmt::Call` with `CallArg`s, which
`.AST` did not convert at all; it is now the same `Call::Name` an expression call
is. `RakuAST::ColonPair::True` / `False` also gained their `.key` accessor.

The measurement also exposed a silent wrong answer: a bare block in statement
position (`{ say "blk" }`) lowered to a closure *value* that nothing called, so
`EVAL Q[{ say "blk" }].AST` printed nothing where rakudo prints `blk`. It now
lowers to the parser's `Stmt::Block`; a block that takes arguments (placeholders,
`@_`, `%_`) stays a closure value.

With these, 1019 of 5672 test files round-trip. The smiley pragmas
(`use variables :D`) are refused rather than rendered wrongly: the parser keeps
`:D` as a string, not a colonpair. Pinned by `t/rakuast/rakuast-use-statement.t`.
