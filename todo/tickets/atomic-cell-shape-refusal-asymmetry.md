# Atomic/captured-cell promotion refuses Array/Hash but accepts Seq/Slip — asymmetric, worth auditing

Found 2026-08-19 while investigating `todo/deep/inline-start-blocks-clobber-a-later-declared-variable.md`
(now resolved, moved to `news/`), during an experimental design that was NOT adopted (caller-side
`ContainerRef` unification for `cas` targets — see that news entry for why it was rejected as this
ticket's fix).

## The observation

`box_captured_lexicals` / `box_decl_local_cell` / `atomic_scalar_cell` (`src/vm/vm_register_ops.rs` and
`src/runtime/builtins_atomic_shared.rs`) refuse to promote a captured local into a shared cell when its
current value is an Array, Hash, Package, Sub, or Proxy (`vm_register_ops.rs:1004-1015`) — but Seq and
Slip are NOT in that refusal list, so a slot holding a Seq/Slip DOES get boxed.

This asymmetry produced a real, reproducible bug in an experimental design: `cas $acc, -> @c { flat @c,
1 }` called twice sequentially (`t/cross-thread-shared-var-writeback-coherence.t` subtest 2) starts each
round with the slot holding an Array (refused — stays on the legacy name-keyed atomic lane) but the
lambda `flat @c, 1` returns a **Seq**, so by the second call the slot holds a Seq and DOES get boxed at
the next closure's creation — silently switching cross-thread reconciliation mechanisms mid-sequence for
the same variable. Nothing retires the legacy-lane mapping when this switch happens (only
`atomic_scalar_cell`'s own promotion path runs the seed-and-retire protocol), so the two lanes can
disagree about the current value.

## Why this wasn't fixed here

The clobber-bug ticket did not touch this path (the shipped fix keeps `cas` entirely off the
cell-promoting lane, deliberately, per commit 85a43994e — this asymmetry only matters if something
DOES try to promote a cas-touched slot, which the shipped fix does not do). It's recorded here as a
latent inconsistency to audit independently, since it could bite a different feature that captures a
Seq/Slip-valued local across threads.

## Next steps

1. Decide whether Seq/Slip SHOULD be excluded from cell promotion the same way Array/Hash are (the
   refusal list's own rationale — likely "these needs escape analysis on their own reified/lazy state,
   not a simple copy-into-a-cell" — probably applies equally to Seq/Slip; check `vm_register_ops.rs`'s
   comments near the refusal list for the original reasoning), or whether Seq/Slip's current inclusion
   is intentional and the bug is elsewhere.
2. If exclusion is the right fix, add Seq/Slip to the refusal list and verify nothing currently depends
   on Seq/Slip being cell-promotable (grep for tests exercising captured Seq/Slip mutation across
   threads).
3. Either way, audit whether ANY promotion path (`box_captured_lexicals`, `box_decl_local_cell`,
   `atomic_scalar_cell`) needs to retire a stale legacy-lane mapping when a promotion happens
   mid-sequence for the same name, independent of the Seq/Slip question — the missing
   seed-and-retire step is the deeper mechanism gap this observation surfaced.
