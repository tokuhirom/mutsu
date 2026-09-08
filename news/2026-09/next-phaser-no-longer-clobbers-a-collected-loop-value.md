# A `NEXT` phaser no longer clobbers a value-position `for`'s result

```raku
my @seen;
my @doubled = do for 1, 2, 3 {
    NEXT { @seen.push: 'next' }
    $_ * 2
};
say @doubled;   # raku: [2 4 6]   mutsu: the pushes, once per iteration
```

The loop-phaser lowering (`Compiler::expand_loop_phasers`) implements `NEXT` by
**appending the phaser body to the end of the loop body**, and splicing it in at
every `next` target as well. In statement position that is invisible: the body's
trailing value is sunk. In expression position it is not — the shared `for`
lowering collects the body's *last statement* as that iteration's result, and
after the expansion the last statement is the phaser body.

`LEAVE` is spliced by the same rewrite and had the same defect
(`do for 1, 2 { LEAVE { 77 }; $_ + 100 }` collected `77, 77`). `FIRST` and `LAST`
were never at risk: `FIRST` is prepended as a guarded `if`, and `LAST` lands in
the post-loop statements.

## The mechanism already existed

`expand_loop_phasers` already captured the body's trailing expression into a temp
and re-emitted it — for KEEP/UNDO (`result_var`) and POST (`post_topic_var`), and
for the gather-lowered `Stmt::Take` form. Two things were wrong with it rather
than missing:

- It was keyed off **which phasers were present**, not off whether anyone wanted
  the value. A body with only a `NEXT` phaser allocated no temp at all.
- Its re-emit sat **before** the appended `leave_ph`/`next_ph`, so even a
  KEEP/UNDO body — which did have a temp — still ended with the phaser body as its
  last statement.

So the fix threads a `wants_value` flag through: `ForParts::collect` is exactly
"this caller collects the body's trailing value", and the statement forms
(`while`, `repeat`, C-style `loop`) pass `false`. When it is set, a capture temp is
allocated even with no KEEP/UNDO/POST, and the re-emit moves to *after* every
appended phaser body.

The `take`n case keeps its existing exemption: when the body's value was already
`take`n into an enclosing gather it is not re-emitted, because sinking a taken
`Failure` would throw.

## Verified against rakudo

`t/loop-phaser-value-position-result.t` pins twelve shapes and passes unchanged on
rakudo v2026.07 — the two bugs (`NEXT`, `LEAVE`, each also asserting the phaser
still ran every iteration), and the neighbours that were already correct and had to
stay so: statement position, `FIRST`, `LAST`, a phaser-free value-position `for`,
`KEEP`, `NEXT`+`KEEP` together, a `gather`/`take` body with `NEXT`, and `next` as
control flow (which still skips its iteration entirely). The ticket's own named pin
`t/control-construct-value-position.t` also still passes.

This predates the statement/expression unification: the old duplicated
`compile_do_for_expr` called `expand_loop_phasers` the same way, so value position
has always been wrong here and statement position has always been right.

Closes #7592.
