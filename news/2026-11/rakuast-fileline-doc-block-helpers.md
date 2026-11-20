# RakuAST: `Str.AST` units are numbered like EVALs; doc-block helpers

`Str.AST` now parses its source as a unit numbered with `EVAL`'s counter, as
rakudo 2026.11 does: `Q[say 1].AST.fileline` is `EVAL_0:1` (mutsu answered
with the running script's file), `Origin::Source.original-file` answers the
unit's name, and `EVAL` and `.AST` share one sequence. `$*CU` keeps naming
the file being compiled.

`StatementList.add-doc-block` and `insert-doc-block` put a Pod block among a
list's statements, and `code-statements` now leaves Pod blocks out, parsed or
added (#14916).
