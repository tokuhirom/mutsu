# ADR-0034 phase 5: Miri probes for `SeqBody`'s `SyncUnsafeCell` usage

[ADR-0034](../../docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md)
(`Accepted`, implemented) landed phases 1-4 (the `SeqBody`/`SeqSource`/`SeqState` representation,
the reify/consume split, and folding `LazyIoLines` into `SeqSource::IoLines`) but explicitly
skipped phase 5 of its migration plan: a Miri probe module for the `SyncUnsafeCell` +
generation-graveyard pattern `src/value/seq_body.rs` reuses from ADR-0030's `NativeBacking`. This
is now done.

## What landed

`src/value/seq_body_shapes.rs`, modelled directly on `src/value/native_cache_shapes.rs`, with four
probes:

1. `first_reference_survives_a_later_reify` — a `&Vec<Value>` taken via `SeqBody::live_generation()`
   from a not-yet-reified body (reading the empty seed) stays valid and unchanged after a `reify()`
   call pushes the real generation.
2. `retired_generations_are_never_overwritten_in_place` — drives `SeqBody::pull_io_lines_prefix`
   (the one path that grows a body's graveyard past a single real generation, used for
   `IO::Handle.lines`/`.words` streaming-subscript reads) twice, to get two distinct, non-empty
   generations alive at once, then proves the retired one still reads its own original content via
   a raw pointer while a fresh borrow reads the longer, newly-pulled generation.
3. `seq_body_and_its_arc_stay_send_and_sync` — compile-time `SeqBody: Sync` and
   `Arc<SeqBody>: Send + Sync` assertions, mirroring `native_cache_shapes`'s equivalent.
4. `two_arc_clones_observe_one_reify_call` — two `Arc<SeqBody>` clones, one `reify()`d from a
   spawned thread, both observe the write: the original handle's own `reify()` call afterward
   serves the already-reified generation (its `pull` closure panics if invoked), proving no
   redundant pull happened and confirming write visibility across the shared `Mutex<SeqState>` +
   `SyncUnsafeCell<Vec<Box<Vec<Value>>>>` core.

`SeqBody::live_generation` was widened from private to `pub(crate)` so the probe module can take a
reference directly, without going through the `Deref` impl — routing a later raw-pointer cast
through the `Arc<SeqBody>` -> `SeqBody` -> `Vec<Value>` deref-coercion chain trips rustc's
`invalid_reference_casting` lint (it conservatively treats the coercion's *starting* place,
`Arc<SeqBody>`, as the cast's backing allocation, even though the actual referent is the
heap-allocated `Vec<Value>` several derefs down).

`.github/workflows/ci.yml`'s `miri` job filters with a **substring match** on `--lib gc::`, which
does not select `value::native_cache_shapes::` or `value::seq_body_shapes::` (neither path contains
`gc::`) — so both need their own explicit `cargo miri test --lib value::<module>` invocation, or the
gate goes green having run none of their probes. Added the `seq_body_shapes` line alongside the
existing `native_cache_shapes` one and extended the reasoning comment to cover both.

All four probes pass locally under `cargo +nightly-2026-08-01 miri test --no-default-features
--features native --lib value::seq_body_shapes -- --test-threads=1` with
`MIRIFLAGS=-Zmiri-disable-isolation` — no UB found under Stacked Borrows.
