# A closure escaping a `do { }` block can call the block's own `sub` again

`my $m = do { sub helper { 7 }; -> { helper() } }; say $m();` died with
"Unknown function: helper", while the same shape written with a
statement-position bare block (`{ sub helper {...}; &m = -> { helper() } }`)
worked. Both forms snapshot and restore the routine registry around the
block so a nested `sub` stays lexical to it, but only the statement form
(`OpCode::BlockScope`) raised `block_scope_depth` while running the block's
body. `RegisterSub` only stashes an escape-hatch copy of the routine (under
a reserved env key a closure's captured env carries out) while
`block_scope_depth() > 0`, so a `sub` declared inside a value-position
`do { }` (`OpCode::DoBlockExpr`) never got that copy, and a closure calling
it after the block exited found nothing left in the registry.

`exec_do_block_expr_op` now raises `block_scope_depth` for the duration of
its body too, exactly when it already snapshots the routine registry (i.e.
when the body actually declares a routine) — the same condition, so a
`do { }` that declares no `sub` pays nothing extra. This also fixes the
`class` body shape from the same report: a `do { }` at class-body scope that
declares a helper `sub` and installs a method referencing it via
`::?CLASS.^add_method` now finds the helper when the method runs.
