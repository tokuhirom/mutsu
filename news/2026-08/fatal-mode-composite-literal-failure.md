# `use fatal` now explodes a Failure nested inside a list/array/hash literal

`use fatal; my @a = (1, "a".Int, 3); say "reached";` used to build `@a` with
the coercion's `Failure` silently embedded as an element and print
`reached` — real `raku` throws immediately while building `@a`, never
reaching `say`.

The root cause: mutsu's construction-time `fatal_mode` checks (in
`vm_var_assign_local.rs`, `vm_var_assign_set_local.rs`, `OpCode::SinkPopAssign`,
the typed-assignment arm in `vm_exec_dispatch.rs`, and `vm_closure_dispatch.rs`'s
`captured_fatal_mode` replay) all key off a simple assignment RHS being
*itself* an unhandled `Failure` instance. When a `Failure` is produced by one
element of a larger list/array/hash literal, the RHS the checks actually see
is the composite (an `Array`/`Hash` value), which never matches the
`Failure`-instance shape — so the check silently passes.

Fixed by adding a composite-construction-time check: `Interpreter::
explode_if_fatal_failure_in_composite()` (`src/runtime/accessors.rs`) scans
a composite literal's already-evaluated element values for an unhandled
`Failure` and, when `fatal_mode` is active, raises its wrapped exception
right there — before the composite ever becomes a stored value. It is
wired into the four composite-construction opcodes that assemble a
list/array/hash literal from stack values: `exec_make_array_op`,
`exec_make_array_no_flatten_op`, `exec_make_hash_op`, and
`exec_make_hash_from_pairs_op` (all in `src/vm/vm_data_ops.rs`). The hash
variants also look one level into a `Pair`/`ValuePair` element, since a
`key => value` pair carries its Failure in the value slot. The check is
gated on `self.fatal_mode` first, so the common non-fatal path pays only a
single bool check, not a scan of every element.

This covers list literals (`(1, "a".Int, 3)`), bracket-array literals
(including nested ones, since an inner `MakeRealArray` already explodes on
its own elements before becoming an element of the outer array), `my %h =
(...)` (which compiles through the same `MakeArray`/typed-assignment path),
and `%(...)` hash-composer literals.

Verified against real `raku` for all of the above, plus a negative case: a
`Failure` caught and replaced by `try { ... } // fallback` before being
embedded in the composite does not explode, since nothing unhandled ever
reaches the literal.

Out of scope, and left as a residual finding: a `Failure` produced by one
*argument expression* of a plain function/method call (`f(1, "a".Int, 3)`)
still doesn't explode, because call-argument marshalling pops its values
directly off the VM stack rather than going through
`MakeArray`/`MakeHash` first, so there is no single chokepoint to hook the
same way. See `todo/tickets/fatal-mode-does-not-explode-failure-in-call-arguments.md`.

Regression tests: `t/fatal-mode-composite-literal-failure.t`.
