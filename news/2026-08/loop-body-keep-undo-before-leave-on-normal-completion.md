# A loop body's KEEP/UNDO now runs before LEAVE on normal completion, matching raku

`Compiler::expand_loop_phasers` (`src/compiler/helpers_phasers.rs`) builds the
synthetic per-iteration loop body used for `for`/`while`/C-style `loop`
bodies that declare phasers. On normal (uninterrupted) completion of an
iteration, it ran the `LEAVE` phaser *before* dispatching to `KEEP`/`UNDO` --
the opposite of real raku's order.

```raku
my $s = "";
for 1 { LEAVE { $s ~= "L" }; KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; 1 }
say $s;
# raku: KL   mutsu (before this fix): LK
```

This was found while fixing the sibling bug where KEEP/UNDO were never
dispatched at all on a `last`/`next`-interrupted iteration (see the
`keep-undo-loop-last-next.t` coverage for that fix). That investigation
independently verified against real `raku` (Rakudo 2026.06) that KEEP/UNDO
always run before LEAVE, on both the interrupted and the normal-completion
paths -- but only the interrupted path's fix picked the right order; the
pre-existing normal-completion tail of `expand_loop_phasers` still had LEAVE
first.

The fix reorders the tail of `expand_loop_phasers`: the KEEP/UNDO dispatch
`Stmt::If` (selected by definedness of the iteration's trailing value, per
`should_run_success_queue`'s rule -- defined routes to KEEP, undefined to
UNDO) is now pushed onto the synthetic loop body *before* the LEAVE phasers
are appended, not after. A stale comment claiming "LEAVE runs before
KEEP/UNDO" (documenting the old, wrong behavior) was corrected to explain the
real order and why it matches the interrupted-path fix.

New coverage in `t/keep-undo-leave-order.t` exercises `for`, `while`, and
C-style `loop` bodies combining `LEAVE`+`KEEP`+`UNDO`, on both the
KEEP-selecting (defined trailing value) and UNDO-selecting (`Nil` trailing
value) branches, confirming the `KEEP`/`UNDO`-then-`LEAVE` order holds
across all three loop forms that share `expand_loop_phasers`.

Severity was low (narrow phaser combination, side-effect ordering only, no
roast test depended on it), but it was a genuine correctness gap in phaser
semantics worth closing.
