# `.subst` closure replacements run their compiled code

`Str.subst(/re/, { ... })` used to run its replacement block through
`eval_block_value`, which compiled the block's AST again for every match: three
`Compiler::compile` calls per `"aaa".subst(/a/, { "bb" }, :g)`. The block now runs
its own `compiled_code` like any other closure (ADR-0133, #10120).

The call follows Rakudo's `$replacement.count ?? $replacement($/) !! $replacement()`:
the match is passed when the block takes an argument, and `$/`, `$0`.. and `$<name>`
hold the current match while it runs. Instead of cloning the whole environment per
match, only those match-context names are saved and restored; the closure call itself
carries the block's writes to the lexicals it closes over.

A side effect fixes a divergence: a pointy block (`-> $m { "$m$_" }`) now sees the
caller's `$_` rather than the match, as in Rakudo.
