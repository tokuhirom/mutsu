# Two mutsu processes writing the same precomp entry corrupt it, and the corrupt entry aborts the process

`precomp::save_cached_unit` writes atomically — but through a temp file whose
name is derived only from the cache entry:

```rust
let tmp_file = cache_file.with_extension("tmp");   // <hash>.tmp, identical in every process
if fs::write(&tmp_file, &data).is_ok() {
    let _ = fs::rename(&tmp_file, &cache_file);
}
```

Two mutsu processes that load the same module at the same time both write
`<hash>.tmp` and both rename it. `fs::write` is not atomic, so the loser's
bytes can be interleaved with the winner's before either rename runs, and what
lands at `<hash>.bin` is a mix of two encodings. The rename is atomic; the write
into the shared temp file is not, so atomically renaming it buys nothing.

This is reachable in ordinary use — `make roast` runs `prove -j`, and a single
`is_run`/`Test::Util` test has a parent interpreter and a child mutsu process
loading the same module concurrently.

The consequence is worse than a cache miss. `load_cached_unit` reads a length
prefix out of the file and hands the slice to `bincode`, which allocates before
it can fail:

```
memory allocation of 1784363464925575909 bytes failed
   ... bincode ... at ./src/precomp.rs:219
   ... at ./src/runtime/run_modules.rs:431
```

`.ok()?` cannot catch that — the allocator aborts the process (SIGABRT). Seen
2026-07-28 as `cargo test --lib is_run_honors_compiler_include_paths` aborting
the whole `cargo test` run; it reproduced on every run until
`~/.cache/mutsu/precomp` was cleared, and passed immediately afterwards.

## Fix

Two independent parts, both small:

1. **Make the temp file unique per writer** — include the pid (and ideally a
   counter) in its name, so concurrent writers cannot share a buffer. Keep the
   rename.
2. **Bound the decode** so a corrupt entry is discarded rather than fatal.
   `bincode::config::standard().with_limit::<N>()` returns a `DecodeError`
   instead of attempting the allocation, which the existing `.ok()?` then turns
   into a clean cache miss. A cache file is never legitimately large, so the
   limit can be generous.

Worth a regression test that a deliberately corrupted `<hash>.bin` yields a
cache miss instead of aborting.
