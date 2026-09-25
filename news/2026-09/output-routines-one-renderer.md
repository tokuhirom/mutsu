# `say`/`put`/`print`/`note` share one renderer: `note 1/0` and `&put(1/0)` die again

Output went through five separate bodies: the four opcodes `Say`/`Put`/
`Print`/`Note`, plus `builtin_print`, which serves the routine form
(`&say(...)`, an alias `my &s = &say`, ...). Each one applied a different
subset of the checks that must run before a value is printed (#9449):

| code | Rakudo | mutsu before |
|---|---|---|
| `note 1/0` | dies | printed `Inf` |
| `&put(1/0)` | dies | printed `Inf` |
| `put Failure.new("x")` | throws | printed `x` |
| `note Failure.new("x")` | throws | printed `x` |
| `&say(Failure.new("x"))` | throws | printed `x` |

Only the `say` opcode threw on an unhandled Failure, even though the helper's
own comment said all four should. Only `say` resolved bound array elements.
`put` skipped the `Nil` warning that `print` gave. The routine form skipped
Proxy resolution entirely.

`render_output(kind, values)` in `src/vm/vm_data_io_ops.rs` is now the only
renderer. It takes an `OutputKind` of `Say`, `Put`, `Print` or `Note`, and for
every argument of every kind it:

- FETCHes any Proxy inside the argument;
- dies on a zero-denominator Rational;
- throws an unhandled Failure.

After that, `say` and `note` render `.gist`, while `put` and `print` render
`.Str` (with the `Nil`/`Regex` warnings and Junction threading). `put` of a
lone Junction still prints one line per eigenstate. The four opcodes pop their
arguments and call it, and `builtin_print` calls it too.

Writing the regression test turned up a second bug. The renderer's Failure
check read only the `handled` attribute, but `.so`, `.Bool` and
`.handled = True` record "handled" by instance id in a registry. So a Failure
that had been marked handled and then passed into a block still threw when
printed, even though its `.handled` said True. The check now asks
`Value::is_failure_handled()`, the same answer `.handled` and method-call
explosion use.

Regression test: `t/routines/output-routine-forms-agree.t` checks all four
routines, in both the listop and `&routine(...)` forms, against `1/0`, an
unhandled Failure and a handled Failure.
