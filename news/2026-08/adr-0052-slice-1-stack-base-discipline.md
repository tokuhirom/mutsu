# ADR-0052 Slice 1: every construct that runs a body owns a stack base

[ADR-0052](../../docs/adr/0052-a-when-clause-produces-its-value-on-the-stack.md)
records that mutsu transports a `when`/`default` clause's value three different
ways — the `succeed` signal, an interpreter-global side channel, and the
ordinary VM stack — and that the duplication is already observable as a stack
leak. Slice 1 is the preparatory half of the repair: it does not change what a
clause evaluates to, it makes the VM stack impossible to corrupt when one
abandons its body mid-range.

## What was wrong

A matching `when` peeks its body's value into the succeed signal and leaves the
same value on the stack, so each of the ~20 `is_succeed()` handlers has to drop
one copy. The ADR's §1.1(c) probe caught one that did not
(`exec_do_given_expr_op`); that single-site fix landed separately and is pinned
by `t/given-expr-succeed-no-double-push.t`. Two systemic gaps remained.

**Loops only established a stack base when they were collecting.** In
`vm_for_loop_body.rs`, `vm_loop_cstyle_repeat.rs`, `vm_control_ops.rs`'s
`while` and both `vm_for_loop_lazy.rs` variants, `stack_base` was
`Option<usize>` gated on `spec.collect`, and the per-iteration
`stack.truncate(base)` lived inside the `if let Some(coll) = collected` arm. A
sink-position loop therefore never returned to a base at all —
`vm_for_loop_intrange.rs`, the int-range fast path, which by construction only
ever runs with `!spec.collect`, had no base whatsoever. Anything an iteration
left behind piled up one value per pass.

**A CONTROL handler ran its statement range without returning to its base.**
This one was not just latent: `exec_try_catch_op_inner`'s CONTROL-handled
branch truncated to `saved_depth` *before* running the handler and never
afterwards, so a `when` matching inside a CONTROL block left the handler body's
value on the stack, where it became the enclosing block's value.
`my $x = do { last; CONTROL { when CX::Last { 7 } } }` evaluated to `7`; raku
yields an undefined value. Statement position hid it, because the `SinkPop` the
compiler emits after a `try` happened to eat the stray value.

## What changed

Every loop now takes `let stack_base = self.stack.len()` at entry,
unconditionally, and truncates to it in every iteration-ending arm — `Ok`,
`is_succeed`, `is_redo`, `is_next`, `is_last` and `leave` — with the C-style
loop also truncating after its step range. Collection still pops the
iteration's value first; `leave` still pushes its own value, after the
truncation. The `repeat` loop, which never collects and had no base, got one
too. The CONTROL-handled branch now mirrors the normalization its `is_return`
sibling and the CATCH handler already performed: truncate to the base, push
`Nil`.

The rest of the `is_succeed()` consumers were audited against the same shape
and found already correct — `exec_given_op`, `exec_do_block_expr_op`,
`exec_succeed_barrier_op`, the block-local branch runner, the closure-call
boundary and the CATCH handler all own a base and truncate; the
`map`/`grep`/`first` fast paths run bodies through `run_reuse`, which clears
the stack on entry; and the react/`THREAD` sites are call boundaries with no
statement range of their own.

## Pin

`t/when-succeed-stack-base-discipline.t`, 16 assertions, every one of them
verified against `raku` v2026.06 first: the ADR's named `given`-expression
probes (`say "A: ", (given 2 { when 2 { "two" } })` and its `default` twin), a
sink-position stack-neutrality probe per loop flavour, the same per
*collecting* loop flavour, and the two CONTROL cases. Five of the sixteen fail
against the pre-change binary (verified by building it), so the pin has teeth.

Finding the load-bearing loop shape took a detour worth recording. A loop body
whose *own* top-level statements include a `when`/`default` is compiled inside
a `SucceedBarrier`, and that opcode already truncates — so the obvious probe,
`for 1..3 { when 2 { "x" } }` in sink position, never reaches the loop's
`is_succeed` arm at all and passed before and after. The **collecting** form
(`do for ...`) is compiled through `compile_stmts_value`, which emits no
barrier, so there the succeed does reach the loop; and the leak only survives
the loop when the *last* iteration is the one that matches, since any following
iteration's `Ok` arm swept the stray value away. `do for 1..3 { when 3 { 'hit'
} }`, its lazy-`gather` twin, and `do for 1..3 { default { 'hit' } }` are the
three probes that actually caught the old behaviour.

`roast/S04-statements/when.t` and `given.t` stay green, as do the existing
`t/` when/given pins the ADR lists.

## Not fixed here

A *collecting* loop still drops a matching iteration's value, because the
succeed handlers read nothing from the abandoned body:
`do for 1..3 { when 2 { "hit" }; "plain" }` is 3 elements in raku and 2 in
mutsu. That is Slice 3, where the handlers start taking the value from the
stack instead of from the signal. Slices 2-4 remain open.
