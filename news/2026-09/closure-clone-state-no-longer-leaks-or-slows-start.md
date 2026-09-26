# A closure clone's `state` no longer leaks, and `start` stops re-walking it

`Digest::RIPEMD` glues every compression block with
`map {$_[[^5].rotate(++$)]}, ...` and spawns two `start`s per block. The
`{ ... ++$ ... }` block is a fresh closure clone each time, and its anonymous
`state` was keyed by that clone — so the program kept one entry per clone
forever, and every `start` walked all of them ([#9504](https://github.com/tokuhirom/mutsu/issues/9504)).

Three changes:

- **Incremental spawn migration.** Each `start` used to walk the whole
  `state_vars` store, allocating a normalized key string per entry, to seed the
  cross-thread cells — although every entry but the newest had already been
  seeded (`seed_if_absent`) by an earlier spawn. Keys are now recorded as they
  are inserted, and a spawn visits only those.
- **Dead clones are reaped.** A closure whose body declares `state` carries a
  `StateScopeGuard` (`src/runtime/state_scope_reaper.rs`), shared by every Rust
  copy of its `SubData`. When the last copy drops, the guard records the clone
  id; the next time the store grows and the dead ids number a quarter of it,
  one sweep removes their entries and cells — O(1) amortized per dead clone.
  The anonymous-`$` store had a second leak of its own: it folded the scope
  into the key *string* and interned one never-freed `Symbol` per clone. The
  scope now lives in the key's id half.
- **Pre-spawn state reaches the thread.** Since state scalars became cells in
  every mode, the migration's "skip cells" test skipped every variable, so
  `sub h { state $n = 0; ++$n }; h(); h(); say await start { h() }` printed `1`
  (rakudo: `3`). Cells are now published as-is, shared by parent and threads.

Measured on a 4-core container, release build, `tmp/state-start.raku` from the
issue (1000 `await start {1}` after every further 20k clones):

| after clones | before | after |
| --- | ---: | ---: |
| 20k | 10.73 s | 0.18 s |
| 100k | 73.13 s | 0.12 s |

Max RSS of the `glue` loop (one anon-state clone per call):

| calls | before | after |
| --- | ---: | ---: |
| 20k | 49.5 MB | 37.5 MB |
| 160k | 130.7 MB | 39.6 MB |

Pinned by `t/routines/closure/state-closure-clone-lifetime.t`.
