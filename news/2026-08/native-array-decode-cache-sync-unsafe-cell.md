# The native `array[T]` decode cache's release-only miscompilation is fixed (ADR-0030)

`ArrayData::sync_native_items` used to mutate three plain fields
(`items`/`native_dirty`/`native_snapshot`) through a `self as *const Self as
*mut Self` cast while only holding `&self` — a textbook unsound interior
mutation. It compiled and usually worked under `-O0`, but was genuine
undefined behavior under Rust's aliasing model, and manifested as a real
miscompilation in release builds: `t/native-array-storage.t` subtest 6 failed
deterministically in release while passing in debug, and a second repro (a
Raku-side write to one index after an unread native/C-side write to a
*different* index) silently discarded the C write on release
(`7 30` instead of the correct `7 99`).

The fix, designed in [ADR-0030](../../docs/adr/0030-native-array-decode-cache-interior-mutability.md),
replaces the three flat fields with a single `Option<Box<NativeBacking>>`,
where `NativeBacking` holds the shared payload node plus a decode cache behind
a new `SyncUnsafeCell<T>` primitive (`src/value/sync_cell.rs`) — mutsu's
second interior-mutability mechanism alongside ADR-0013's `gc_contents_mut`,
for the specific shape ADR-0013 excluded: a read-path cache fill performed
under a shared borrow the caller keeps using, rather than a handle-holding
structural write.

A "generation graveyard" makes the design sound rather than merely
well-typed: a re-sync **pushes** a fresh `Box<Vec<Value>>` instead of
overwriting the live one, so a `&Vec<Value>` handed out by an earlier
`items()` call stays valid across a later re-sync — the obligation ADR-0013's
`gc_contents_mut` discharges by auditing ~62 enumerable call sites cannot be
discharged the same way here, since `Deref for ArrayData` puts the same
obligation on every `&ArrayData` in the interpreter. Every `&mut self` entry
point (`items_mut`, `take_items`, `into_items`) prunes the graveyard back to
one generation, since the borrow checker there already proves no shared
borrow into any generation is live.

Landed in two PRs: the two plain logic bugs underneath the UB (`items_mut`
not syncing before marking dirty, and a redundant full-buffer clone on every
read) shipped first as #6666; the representation change, the `SyncUnsafeCell`
primitive, a new `value::native_cache_shapes` Miri probe module (five probes
covering the core aliasing shape, generation stability, pruning soundness,
`Sync` posture, and `Clone` independence), and a widened Miri CI filter
(`--lib gc::` does not match `value::native_cache_shapes::` by substring)
shipped together as the representation-change PR.

## Effect

- `t/native-array-storage.t`: 8/8 on both debug and release (previously 7/8
  on release, subtest 6 failing).
- The ADR's second repro (`@a[0] = 7` after an unread `$payload[2] = 99`)
  prints `7 99` on release, matching debug — previously `7 30`.
- `cargo +nightly-2026-08-01 miri test --lib value::native_cache_shapes`: all
  5 probes pass, including the exact shape that was UB before this fix.
