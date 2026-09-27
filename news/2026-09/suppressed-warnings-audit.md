# Suppressed-warning audit: dead code removed, `alloc-stats` linted

The default, `jit`-off, wasm32 and rustdoc lint configurations were already
warning-free, but two things were hiding warnings from them.

**The `alloc-stats` feature was linted by nobody.** Under
`cargo clippy --features alloc-stats --all-targets` the library had a
`let_unit_value` warning, and `tests/source_file_sym_fallback_alloc_budget.rs`
did not compile at all: it installs its own counting `#[global_allocator]`,
which conflicts with the one the feature installs in the library. The test is
now compiled out under that feature, and the configuration is a fifth pass in
`make lint` and in CI's `lint-configs` job.

**`#[allow(dead_code)]` had become a blanket.** Removing every
`#[allow(dead_code)]` / `#[allow(unused_imports)]` showed about 50 items that
were really dead. Deleted: a 170-line `regex_match_from_in_pkg` superseded by
the current matcher, a private copy of `civil_to_epoch_days` plus a leap-second
table nothing called, unused error constructors (`X::Syntax::Missing` /
`Confused` / `Malformed` helpers), an unused `OutputSink` guard re-export, and
write-only fields (`RoleDef::is_rw`, `CustomTypeData::is_mixin`,
`Interpreter::pending_regex_error`, the async listener's `host`/`port`, ...).
About 35 allows guarded items that are used today and were simply dropped.

The allows that remain are each documented: the GC root-enumeration surface
kept for the full-root verify mode, NativeCall RAII keep-alive fields, and
ADR-0019 dispatch scaffolding exercised only by unit tests. One of them hid a
real bug: `DestructureVar::is_optional` is parsed and never read, so
`my ($a, $b) := (1,)` binds without Raku's "Too few positionals" error
(#9763).
