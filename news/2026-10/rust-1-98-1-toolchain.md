# Rust toolchain bumped to 1.98.1

Every stable CI pin (`ci.yml`, `bench.yml`, `release.yml`, `tag-release.yml`,
`ecosystem-sweep.yml`), `.mise.toml`, the Docker builder image and both
manifests' `rust-version` now target Rust 1.98.1 (from 1.96.x).

The new compiler's clippy flagged fourteen sites. `chunks_exact(N)` with a
constant `N` became `as_chunks::<N>()`, which hands out fixed-size arrays: the
SHA-1 compression function now takes `&[u8; 64]`, and the UTF-16 decoders build
code units with `u16::from_*_bytes(chunk)` directly. A late-initialised pair in
the regex `my token` scanner became one `let` of an `if` expression. The two
`drain(..).collect()` calls in the profiler's sample fold keep an `allow`,
since `mem::take` would give away the capacity reserved at arm time.

The Miri job's nightly pin is unchanged; it is bumped deliberately in its own
PR.
