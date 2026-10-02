# A quick gate for the remote container, and one integration-test binary

On the ~4-core remote container, the full pre-publication gate was the most expensive part of a
session. It compiled the crate about seven times (four clippy configurations, rustdoc, release,
the debug test build) and then ran the whole TAP suite and the roast whitelist on the release
binary. That took longer than the CI run that repeats all of it on parallel runners. Two changes
cut that cost.

**`scripts/dev gate` has a quick profile**, and it is the default when `CLAUDE_CODE_REMOTE=true`
(ADR-0126, amendment 2026-10-02). It runs `checks`, `fmt`, the default clippy and the debug
`cargo test`. It then runs `prove` on the debug binary that `cargo test` already built, over
these files:

- the `t/` and `roast/` files the branch touches, measured against a freshly fetched
  `origin/main`;
- the files the branch adds to `roast-whitelist.txt`;
- whatever `--focus PATH...` names.

Per-file timeouts are scaled ×4 for the debug build. No release binary is built. The report
records the profile and the focus, so a PR body that quotes `scripts/dev status` says which gate
passed. `--full` still runs everything. On this branch, from a warm dependency cache and a cold
`mutsu` debug build, the quick gate took about 11 minutes: checks 57 s, fmt 13 s, clippy 148 s,
cargo test 433 s, and the 60 focus files 7 s. A single cold release build alone takes 10m38s on
the same box.

**The integration tests are two binaries instead of forty-two.** Each `tests/*.rs` file was its
own test target, and each one linked the whole library. Now `tests/integration.rs` declares every
file as a module. The three allocation-budget tests, which each install a counting
`#[global_allocator]`, share `tests/alloc_budget.rs`; a binary may link only one global
allocator. The files stay where they were, so the `tests/<name>.rs` paths quoted across `src/`
and `docs/` remain valid. `autotests = false` stops cargo from also building them one by one.
A new `make check-integration-tests` guard, in `make checks` and CI, fails when a `tests/*.rs`
file is declared by neither root, because such a file would otherwise silently never run.

After touching one source file, the incremental `cargo test --no-run` went from 52–55 s to
39–40 s. A cold build barely changes (4m38s → 4m29s), because there the links overlap with the
library's own compile.

Two other ideas were measured and dropped:

- The `cdylib` crate type on the native build costs nothing measurable. A/B release rebuilds
  took 513 s and 511 s with it, and 500 s and 524 s without it, and the debug builds were equal
  too. Moving the wasm `cdylib` into a crate of its own would only have added complexity.
- Switching the linker to lld was already done for us. rustc defaults to rust-lld on
  x86_64 Linux, and the binaries' `.comment` section reads `Linker: LLD 22.1.8`.
