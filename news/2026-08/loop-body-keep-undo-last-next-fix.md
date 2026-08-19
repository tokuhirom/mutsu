# KEEP/UNDO phasers now dispatch correctly when a loop iteration is interrupted by `last`/`next`

A `KEEP`/`UNDO` phaser declared inside a `for`/`while`/`loop` body used to
never run at all when the iteration was interrupted by `last` or `next` --
neither queue was dispatched, not merely mis-decided:

```raku
my $s = "";
for 1 { KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; last }
say "[$s]";
$s = "";
for 1,2 { KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; next }
say "[$s]";
```

Real raku prints `[U]` then `[UU]` (an interrupted iteration's trailing
value is undefined, which per the definedness rule already implemented for
bare blocks -- `should_run_success_queue`/`should_run_success_queue_raw`/`_vm`
-- always routes to UNDO, never KEEP). mutsu printed `[]` then `[]`: the
dispatch was simply never reached.

## Root cause

`Compiler::expand_loop_phasers` (`src/compiler/helpers_phasers.rs`) extracts
`KEEP`/`UNDO`/`LEAVE`/`NEXT` phaser bodies out of a loop body and re-emits
the KEEP/UNDO dispatch as an ordinary `if` statement placed AFTER the loop's
main statements, in the per-iteration synthetic body. When `last`/`next`
fires, it throws a control-flow `RuntimeError` that unwinds straight out of
the loop body -- skipping every subsequent statement, including that
KEEP/UNDO dispatch `if`, since it never gets a chance to run.

Existing infrastructure (`rewrite_next_targets_in_stmt`) already handled this
correctly for `LEAVE`: it rewrites every `Stmt::Last`/`Stmt::Next` in the
loop body into a small synthetic block that runs the LEAVE phasers
immediately before the actual `last`/`next`. But `KEEP`/`UNDO` were not
threaded through that rewrite at all, and the rewrite was skipped entirely
whenever neither `LEAVE` nor `NEXT` phasers were declared -- exactly the
common case of a loop body with only `KEEP`/`UNDO`.

## Fix

`rewrite_next_targets_in_stmt`/`rewrite_next_targets_in_stmts` now take an
`undo_ph` parameter and inject it (never `keep_ph` -- an interrupted exit's
value is always undefined, so it is always UNDO, never KEEP) into the
synthetic wrapper built around an interrupted `last`/`next`. The gate that
decides whether to run the rewrite at all was extended to also fire when
`UNDO` phasers are present.

While verifying the fix against real `raku` (Rakudo 2026.06), the ordering
between `LEAVE`, `KEEP`/`UNDO`, and `NEXT` was checked directly rather than
guessed:

- Normal (uninterrupted) completion: KEEP/UNDO, then LEAVE, then NEXT
  (`KLN`).
- `last`-interrupted exit: UNDO, then LEAVE (`UL`) -- same relative
  KEEP/UNDO-before-LEAVE order as normal completion.
- `next`-interrupted exit: NEXT, then UNDO, then LEAVE (`NUL`) -- the
  OPPOSITE order from normal completion, because an explicit `next`
  statement runs its NEXT phasers synchronously as part of the `next`
  transfer itself, before the block-exit unwind (UNDO, then LEAVE) proceeds.

The fix reproduces this exact ordering for the new interrupted-exit dispatch
(and, as a side effect of restructuring the same wrap, also corrected a
pre-existing LEAVE-vs-NEXT ordering bug for explicit `next` with no
KEEP/UNDO declared -- mutsu previously ran LEAVE before NEXT there; raku
runs NEXT before LEAVE).

A separate, narrower pre-existing bug was found during verification and
recorded rather than fixed here (different code path, out of scope for this
fix): on *normal* completion, mutsu's `expand_loop_phasers` still runs LEAVE
before KEEP/UNDO instead of after --
see `todo/tickets/loop-body-leave-runs-before-keep-undo-instead-of-after.md`.

## Tests

New regression test `t/keep-undo-loop-last-next.t` covers `for`/`while`
bodies interrupted by `last`/`next`, normal-completion KEEP as a regression
guard, and the verified LEAVE/KEEP/UNDO/NEXT orderings for both interrupted
exit kinds. Every assertion in the new test was independently verified to
pass against real `raku` before being pinned.
