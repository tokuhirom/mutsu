# A unit-level BEGIN runs first, over static-state lexicals

This is slice 1 of [ADR-0134](../../docs/adr/0134-begin-time-prologue.md). A
compilation unit's top-level `BEGIN` now runs before any of the unit's
run-time code. The unit can be the program, a module or an EVAL string. The
`BEGIN` runs in source order together with the declarations it can observe,
and it runs in the unit's own frame.

It sees each lexical in its static state: declared, but with no run-time
initializer applied yet. So `my $c = True; BEGIN say $c.raku` prints `Any`,
and `my @a = 9; BEGIN @a.push(1); say @a` prints `[9]`, both matching rakudo.
Before this change they printed `Bool::True` and `[9 1]`.

A class, sub or constant declared earlier in the unit is available to the
`BEGIN`. When the undeclared-routine check rejects the unit, the `BEGIN` still
runs first, so its output comes before the compile error.

This replaces the sub-interpreter hoist `run_toplevel_begin_phasers`. That
hoist pre-ran a top-level BEGIN only when a scan of its AST's debug text found
no call, bareword or `use`, and it copied the values back into the mainline.
A module's top-level BEGIN, which used to run in place, now gets the same
prologue.
