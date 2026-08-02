# Close ADR-0013: add the Miri gate, and fix the stale `aliased_mut.rs` header

Extracted from PLAN.md §2.1 (2026-08-02).

[ADR-0013](../../docs/adr/0013-container-interior-mutability-cellvalue.md) fixed the provenance UB in
the GC's aliased writes: per §7 the `UnsafeCell` was placed in `GcBox.value` (`src/gc/gc_ptr.rs`)
instead of wrapping each container payload, so **every** `Gc<T>` is interior-mutable at the primitive
and all the aliased-write sites (`gc::gc_contents_mut`, `Gc::{get,make}_mut`) became
provenance-sound, with no call-site or `Value`-representation churn.

Two loose ends remain:

1. **The Miri gate (ADR-0013 §4 phase 4) is missing.** There is no `miri` job in
   `.github/workflows/`, so the soundness claim rests on an argument rather than a check. Add it
   informational-first, then blocking; pin a nightly matching the crate's stabilized-feature usage.

2. **`src/value/aliased_mut.rs`'s module header is stale** — it still documents the provenance
   violation as live and names Track B as the future fix. Both have been false since ADR-0013
   (Track B is no longer fused with this work either, per ADR-0001 §7). Rewrite the header.

The narrow cross-thread data race (an aliased write to a genuinely shared target) stays deferred to
layer 3c by decision — it is not what this ticket closes.

## Background already settled (do not redo)

The 2026-07-20 inventory ([docs/gc-contents-mut-inventory.md](../../docs/gc-contents-mut-inventory.md))
read and classified every production site — **54 sites / 20 files**: provably-unique = 3,
`make_mut`-COW-coverable = **0**, needs-first-class-cell = 51. So "route easy clusters through COW"
is a dead end; it can retire zero sites. The three provably-unique sites carry
`debug_assert_eq!(strong_count(), 1)`, and `Gc::verify_unique_for_aliased_mut` machine-checks the
`strong == 1 ⟹ unique` argument under `MUTSU_GC_VERIFY=1` (pins: `gc_ptr` unit tests
`arc_and_gc_strong_counts_stay_in_lockstep` / `erased_clone_makes_arc_exceed_gc_strong`).
