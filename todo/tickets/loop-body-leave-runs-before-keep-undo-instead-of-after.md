# A loop body's LEAVE phaser runs BEFORE KEEP/UNDO instead of after, on normal (uninterrupted) completion

Found while fixing
`todo/tickets/loop-body-keep-undo-not-run-on-last-next.md` (KEEP/UNDO never
dispatched on a `last`/`next`-interrupted loop iteration) and verifying
phaser ordering against real `raku`. This is a separate, pre-existing bug:
the *relative order* between LEAVE and KEEP/UNDO is backwards on the
already-working normal-completion path, for both `for` and (presumably)
`while`/`loop` bodies.

## Repro

```raku
my $s = "";
for 1 { LEAVE { $s ~= "L" }; KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; 1 }
say $s;
```

```
raku:  KL
mutsu: LK
```

Verified with the local `raku` (Rakudo 2026.06): KEEP always runs BEFORE
LEAVE on normal completion of a loop body (`todo/tickets/loop-body-keep-undo-not-run-on-last-next.md`'s
investigation independently confirmed this same KEEP-before-LEAVE order also
holds for the `last`-interrupted case, i.e. UNDO before LEAVE there too --
that half is already fixed). Only the *normal* (uninterrupted) fall-through
path still has LEAVE before KEEP/UNDO.

## Root cause (partial investigation)

`Compiler::expand_loop_phasers` in `src/compiler/helpers_phasers.rs` builds
the per-iteration synthetic loop body. Near the end of that function:

```rust
// LEAVE runs before KEEP/UNDO (in reverse declaration order)
loop_body.extend(leave_ph);
if let Some(result_var) = result_var.clone() {
    if !keep_ph.is_empty() || !undo_ph.is_empty() {
        loop_body.push(Stmt::If {
            cond: Expr::Var(result_var.clone()),
            then_branch: keep_ph,
            else_branch: undo_ph,
            ...
        });
    }
    ...
}
```

The comment ("LEAVE runs before KEEP/UNDO") states the CURRENT (wrong)
behavior, not the raku spec -- it should be the other way around: the
KEEP/UNDO dispatch `Stmt::If` should be pushed onto `loop_body` before
`leave_ph` is extended, not after.

## Why this is a separate ticket

The KEEP/UNDO-not-run-at-all ticket is about the interrupted (`last`/`next`)
path never reaching the KEEP/UNDO dispatch at all; its fix (in the same
`expand_loop_phasers` function, via `rewrite_next_targets_in_stmt`) had to
independently choose an order for the NEW dispatch it injects, and matched
it against real raku (UNDO before LEAVE for `last`; NEXT, then UNDO, then
LEAVE for `next`). This ticket is about the DIFFERENT, pre-existing bug in
the normal-completion tail of the same function, which the other ticket's
fix deliberately did not touch (different code path, different set of
statements, no reason to co-mingle the fix).

## Severity

Low: narrow (a LEAVE phaser combined with KEEP/UNDO in the same loop body is
an unusual pairing) and cosmetic (side-effect ordering only; the correct
queue -- KEEP vs UNDO -- is already selected correctly, just interleaved with
LEAVE in the wrong order). No roast test currently depends on this.
