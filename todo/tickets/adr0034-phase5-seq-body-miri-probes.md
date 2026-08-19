# ADR-0034 phase 5: add Miri probes for `SeqBody`'s `SyncUnsafeCell` usage

[ADR-0034](../../docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
(`Accepted`, implemented) landed phases 1-4 (the `SeqBody`/`SeqSource`/`SeqState` representation,
the reify/consume split, and folding `LazyIoLines` into `SeqSource::IoLines`) but explicitly
skipped phase 5 of its migration plan: a Miri probe module for the `SyncUnsafeCell` +
generation-graveyard pattern `src/value/seq_body.rs` reuses from ADR-0030's `NativeBacking`.

This is soundness-probe test infrastructure, not a functional gap — `t/` and roast already cover
`SeqBody`'s behavior extensively. But the ADR's own risk section (§5) is explicit that the
`Deref`-through-generations trick needs verification under Miri's Stacked Borrows model, the same
way `src/value/native_cache_shapes.rs` verifies `NativeBacking`'s.

## What to do

Add `src/value/seq_body_shapes.rs`, modelled directly on `src/value/native_cache_shapes.rs`
(168 lines — read it first), probing:

1. A `&Vec<Value>` reference taken from `SeqBody::live_generation()` (or via `Deref`) BEFORE a
   `reify`/`take` call, still read AFTER that call succeeds — proves the generation graveyard (never
   overwriting an existing slot, only pushing a new one) keeps the earlier borrow valid.
2. The retired (non-live) generation still reads as its original content while a fresh borrow of
   the new live generation sees the newly-pulled elements — two borrows of two different
   generations, both valid simultaneously.
3. `SeqBody: Sync` and `Arc<SeqBody>: Send + Sync` (compile-time assertions, same as
   `native_cache_shapes.rs`'s equivalent).
4. Two `Arc<SeqBody>` clones both observe one `reify()` call (write visibility across the shared
   `Mutex<SeqState>` + `SyncUnsafeCell<Vec<Box<Vec<Value>>>>`).

Then extend `.github/workflows/ci.yml`'s `miri` job: per ADR-0030 §5, the job's filter is a
**substring match** on `--lib gc::`, which silently selects nothing new for a module whose path
doesn't contain `gc::` — `native_cache_shapes` needed its own explicit invocation line, and
`seq_body_shapes` will too. Read the existing `native_cache_shapes` step comment in that job before
adding the new one, and extend the reasoning comment rather than duplicating it blindly.

## Why this wasn't done in the ADR-0034 PR

The PR that implemented phases 1-4 ran very long (an interrupted, resumed, multi-hour session) and
surfaced several unanticipated correctness bugs along the way (see the ADR's §7.1 Outcome) that
took priority over the soundness-probe infrastructure. Filed here rather than left to evaporate.
