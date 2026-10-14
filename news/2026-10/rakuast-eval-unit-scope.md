# `RakuAST::Node.EVAL` runs as a compilation unit of its own

`Q[my $q = 1].AST.EVAL` used to leave `$q` behind in the caller, so a later
`EVAL q[$q]` printed `1` where rakudo reports `Variable '$q' is not declared`
(#13274). The lowered tree ran through `eval_block_value` directly, skipping
everything the string `EVAL` does to keep a unit's declarations inside it.

The string `EVAL` pipeline is now split into reusable pieces:
`isolated_eval_unit` (the snapshots and restores of routines, roles, classes,
lexical code variables, pragmas and the topic), `run_eval_unit_stmts` (phaser
reordering, `our` routine installation and the cleanup of the unit's own `my`
lexicals) and `eval_eval_source` (parsing the string, with its retries). A
RakuAST `EVAL` lowers its tree and runs it through the first two, inside the
same import scope and compunit as a string `EVAL`. A tree's `my` variables and
subs no longer leak, a `my` that shadows a caller variable leaves the caller's
value alone, and an assignment to a caller variable still writes through.

`t/rakuast/rakuast-eval-scope.t` pins it. One expectation in
`t/rakuast/rakuast-declaration-headers.t` had only passed because of the leak:
`callsame` from a class that `hides` its parent finds no candidate (rakudo:
`Nil`), and the test now says so.
