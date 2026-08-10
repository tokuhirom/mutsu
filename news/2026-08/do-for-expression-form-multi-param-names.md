# The `do for LIST -> $a, $b { ... }` expression form now carries multi-param metadata

`do for @list -> $x, $y { start { ... } }` (or any `for` used as the tail
value of `try`/`do`/a sub body) used to silently drop its pointy-block
parameter names when compiled: `ForLoopSpec::multi_param_names`,
`multi_param_locals`, `multi_param_type_constraints`, and `rw_param_names`
were all hardcoded empty on this path, even though the equivalent
**statement** form (`for @list -> $x, $y { ... }`, no `do`) built them
correctly. Any VM mechanism keyed off `multi_param_names` was silently
inert here — most importantly, ADR-0023's cross-thread bare-name-lane
masking, so a `do for` loop with a multi-param pointy block spawning
sibling `start {}` threads was exposed to the same collision class of bug
ADR-0023 fixed for the statement form: both threads would converge on the
last iteration's values instead of each keeping its own.

Root cause: `ast::Stmt::For` already carries a `params_def: Vec<ParamDef>`
field (aligned with `params`) for exactly this purpose, but the
expression-form compile path (`expr_block.rs`'s match arm dispatching to
`compile_do_for_expr`) never extracted it from the `Stmt::For` pattern —
it destructured with `..`, so the field was silently discarded before it
ever reached `helpers_do_expr.rs`. The fix threads `params_def` (and
`rw_block`) through to `compile_do_for_expr`, which now mirrors
`stmt.rs`'s construction of `multi_param_names`/`multi_param_locals`/
`multi_param_type_constraints`/`rw_param_names`/`kv_mode`/`has_rw`/
`has_copy` for the expression form.

New pin: `t/for-loop-param-start-sibling-isolation.t` subtest 7 (the
multi-param `do for` variant, mirroring subtest 5's already-passing
statement form).
