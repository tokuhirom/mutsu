# Toolchain bumped to Rust 1.99.0

Rust 1.99.0 was released on 2026-10-01, and mutsu now builds with it everywhere.
The minimum supported Rust version (`rust-version` in both manifests) goes up with it.
The bump touches every workflow's `dtolnay/rust-toolchain` pin (CI, bench, release,
tag-release, stress, ecosystem sweep), `.mise.toml`, the Docker builder image
(`rust:1.99-bookworm`), the README, the site's install instructions and the
`rustc-too-old` skill. The remote-session start hook reads `rust-version` from
`Cargo.toml`, so new sessions install 1.99.0 automatically.

The new toolchain raised four warnings, all fixed:

- `Atomic*::fetch_update` is deprecated in 1.99, renamed to `try_update` with the same
  signature and semantics. Both callers now use `try_update`: the stack-budget
  reservation (`src/runtime/stack_budget.rs`) and the GC's approximate buffered-roots
  counter (`src/gc/gc_ptr.rs`).
- Clippy 1.99 newly reports `needless_borrows_for_generic_args` on `Option::map(&closure)`:
  two such calls in `splice` argument resolution
  (`src/runtime/methods_call_helpers.rs`) now pass the closure by value.

The release's one compatibility note warns against turning a `Box::leak`ed reference
back into a `Box`. It does not affect mutsu, which never calls `Box::from_raw`.
