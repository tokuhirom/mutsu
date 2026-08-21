# `use fatal` now explodes a Failure produced by a call's argument expression

`use fatal; sub f($a,$b,$c) { say "in f" }; f(1, "a".Int, 3); say "reached";`
used to call `f` with the coercion's `Failure` silently bound to `$b`,
printing `in f` and then `reached`. Real `raku` explodes right at the call
site — `f`'s body never runs, `reached` is never reached.

This was the residual scope left after the earlier list/array/hash
composite-literal fix (`news/2026-08/fatal-mode-composite-literal-failure.md`):
that fix hooks `Interpreter::explode_if_fatal_failure_in_composite()`
(`src/runtime/accessors.rs`) into the four opcodes that assemble a
composite from stack values (`exec_make_array_op` and friends in
`src/vm/vm_data_ops.rs`), but a plain function/method call's arguments are
**not** assembled through `MakeArray`/`MakeHash` first — each call opcode
(`CallFunc`, `CallFuncNamed`, `CallMethod`, `CallMethodDynamic`,
`CallMethodDynamicMut`, `CallMethodMut`, `CallOnValue`, `CallOnCodeVar`,
`ExecCall`, `ExecCallPairs`, `HyperMethodCall`, `HyperMethodCallDynamic`)
pops its `arity` argument values straight off the VM stack inside its own
`exec_call_*_op` handler, with several of them further splitting into
fast/light/cached dispatch sub-paths that each drain the stack their own
way.

Rather than duplicating a scan into every one of those internal drain
sites, the fix adds a single new helper,
`Interpreter::explode_if_fatal_failure_in_call_args(arity)` (also in
`src/runtime/accessors.rs`), and calls it once per opcode **at the
dispatch level in `src/vm/vm_exec_dispatch.rs`**, immediately before
delegating to the opcode's `exec_call_*_op` handler. At that point the top
`arity` slots of the VM stack are always exactly the call's already-
evaluated argument values (any receiver/dynamic-method-name value the
opcode also needs sits *below* them), regardless of which internal
fast/light/slow sub-path the handler will end up choosing — so one
dispatch-level check covers every internal specialization without touching
their bodies. The helper is gated on `self.fatal_mode` first, matching the
composite-literal fix's cost profile: the common non-fatal path pays a
single bool check per call, not a stack scan.

No double-firing risk: a literal composite passed as one argument
(`f((1, "a".Int, 3)))`) already explodes at its own `MakeArray` time, long
before it becomes one of the call's `arity` values, so the new check never
even sees it. A `try` around a call still behaves correctly too, since
`try` simply forces `fatal_mode` on for its dynamic extent — the new check
just participates in that the same way the composite-literal check does.

Verified against real `raku` for the ticket's repro, a named-argument
variant, a static method-call variant, and a dynamic method-call variant
(`.$name(...)`), plus two negative cases: a Failure caught by `try` before
being passed as an argument does not explode the call, and a non-fatal-mode
program still passes a Failure argument through as a soft value bound to
the parameter (unchanged behavior).

Regression tests: `t/fatal-mode-call-argument-failure.t` (9 subtests,
including a re-check that `t/fatal-mode-composite-literal-failure.t`'s
scenarios still pass unchanged).
