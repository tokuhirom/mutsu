# Two mutsu processes no longer corrupt each other's precomp cache

`precomp::save_cached_unit` wrote its entry atomically — through a temp file
whose name was derived only from the cache entry:

```rust
let tmp_file = cache_file.with_extension("tmp");   // <hash>.tmp, the same in every process
if fs::write(&tmp_file, &data).is_ok() {
    let _ = fs::rename(&tmp_file, &cache_file);
}
```

The rename is atomic. The write into the temp file is not, and the temp file was
shared by every process on the machine, so two mutsu processes loading the same
module at the same time could interleave their bytes there and rename the
mixture into place. Renaming a shared scratch buffer atomically buys nothing.
This is reachable in ordinary use: `make roast` runs `prove -j`, and a single
`Test::Util` test has a parent interpreter and the child mutsu it spawns loading
the same module concurrently.

The consequence was worse than a cache miss. `load_cached_unit` reads a length
prefix out of the file and hands the slice to `bincode`, which allocates before
it can report a mismatch:

```
memory allocation of 1784363464925575909 bytes failed
   ... bincode ... at ./src/precomp.rs
   ... at ./src/runtime/run_modules.rs
```

`.ok()?` reads as "a bad entry is a cache miss", but it never runs — the
allocator aborts the process with SIGABRT first. Observed 2026-07-28 as
`cargo test --lib is_run_honors_compiler_include_paths` killing the whole
`cargo test` run; it reproduced on every attempt until `~/.cache/mutsu/precomp`
was cleared, and passed immediately afterwards.

Both halves are fixed:

- **The temp file is per-writer**, `<hash>.<pid>.tmp`, so concurrent writers
  cannot share a buffer. The once-per-process cache prune now also sweeps
  scratch files older than an hour, since a process killed between the write and
  the rename leaves one behind and a per-pid name is never reused.
- **The decode is bounded** (`with_limit::<256 MiB>()`), so a corrupt entry
  returns a `DecodeError` that the existing `.ok()?` turns into a clean cache
  miss and a reparse. 256 MiB is far above any real entry and far below "the
  machine's memory", which was the only bound the encoding itself imposed.

Pinned by `precomp::tests::concurrent_writers_do_not_share_a_temp_file` and
`precomp::tests::a_corrupt_entry_is_a_cache_miss_not_an_abort`. The second one
has teeth: it plants a `String` length prefix claiming ~1.7 exabytes (bincode's
varint marker `253`, the shape a prefix read out of unrelated bytes takes), and
with the limit removed it still aborts the test process — verified. A byte that
is *not* a valid varint marker would not do: that errors out before any
allocation is attempted, so it would exercise nothing.
