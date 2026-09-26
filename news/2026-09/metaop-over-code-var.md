# `[&op]` meta-operators use the `&op` term, not `infix:<op>`

`[&name]` and its meta forms now call the callable the `&name` term denotes.
Before, the parser turned `[&name]`, `R[&name]`, `X[&name]` and `Z[&name]`
into `Expr::InfixFunc`, which looks `name` up as an `infix:<name>` operator at
run time. That caused four bugs. `(10,) Z[&op] (9,)` never zipped and answered
`2`. A lexical `&cmp` ran the builtin `cmp`. A `&op` sub parameter was not
found at all, which gave "Two terms in a row" or a wrong answer. And
`X[&sprintf]` needed its own special case in the VM.

All four spellings now go through the same lowering as the `[&TERM]` form: a
call on the `&name` term, or `cross`/`zip` with `:with(&name)`. The
`atan2`/`sprintf` special cases in `exec_infix_func_op` could no longer be
reached, so they are gone. A comma-list right operand of `X[&f]` is now one
list operand, not one list per element.

`»[&op]«` and `[[&op]]` still look their operator up by name at run time.
They now try the frame's own `&op` binding first, through a new
`frame_amp_callable` helper that `GetCodeVar` also uses, so a `&op` parameter
works there too. `[[&l]]` over `&infix:<leg>` (or `cmp`, `<=>`) is now a plain
left fold (`Less` for `10, 9`, as in Rakudo), not a chaining comparison.

Test: `t/lang/operators/metaop-over-code-var.t`. Closes #9464.
