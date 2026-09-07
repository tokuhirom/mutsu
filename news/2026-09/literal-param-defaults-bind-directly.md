# A literal parameter default binds directly instead of being evaluated per call

`Interpreter::eval_param_default` evaluated every omitted parameter's default
through `eval_block_value`: save and restore the topic, shadow the parameter
with its own type object, compile the default's AST as a one-statement block,
run it, restore. That is the right protocol for `$c = $a * 2` or
`&foo = &foo`, whose value depends on the scope the binder has built so far.
It was also paid for `$desc = ''` and `$n = 1`, whose value depends on nothing.

The general binder now binds an immutable scalar literal default (`Int`,
`Num`, `Str`, `Bool`, `Rat`) directly, before any of that machinery runs. It
is the same shape the positional light path already fills from its
registration-time table (`CompiledFunction::const_fill_for_param`,
`news/2026-09/defaulted-params-reach-the-positional-light-path.md`); this
closes the gap for the routines that path cannot admit at all -- an `is copy`
parameter, a coercion type, a `multi` -- which is every assertion routine in
the vendored upstream `Test.rakumod`. A container literal (`$acc = []`) is
still evaluated per call on purpose, because that evaluation is what hands
each call its own fresh container.

## Measured

`proclaim($cond, $desc is copy, $unescaped-prefix = '')` is called once per
assertion under `MUTSU_REAL_TEST=1`, and every call omits the third
parameter. On the callgrind protocol from
`todo/deep/vendor-real-test-module.md` (300 `ok 1, "x"` under the real
module, one-assertion baseline subtracted):

| | per assertion |
| --- | --- |
| before | 492,188 Ir |
| after | 454,016 Ir |

-7.8%, all of it the `eval_param_default` row (38,214 Ir inclusive) going to
zero. Wall clock on the same box: the 20,000-assertion `ok` loop 1.33 s ->
1.0 s, `roast/S03-buf/write-int.t` under the real module 13.8 s -> 11.6 s.

## Pin

`MUTSU_VM_STATS=1` now prints a `param-defaults:` line with an `evaluated=`
count (defaults that went through `eval_block_value`) and a `constant=` count
(literals bound directly). `tests/param_default_literal_binds_directly.rs`
pins `evaluated=0` for a literal default on an `is copy` routine, one
evaluation per call for a default that reads an earlier parameter, and a
fresh container per call for `$acc = []`.
