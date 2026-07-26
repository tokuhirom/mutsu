# `make test` no longer builds a release binary (~28 min → ~12 min)

`make test` used to build **both** a debug and a release binary: debug for the Rust
`#[test]` unit tests, release solely so the ~2469-file `t/` TAP suite ran on an optimized
binary. Measuring the phases on a 12-core box (only the `mutsu` crate recompiling, deps
cached) showed the release build was the entire problem:

| Phase | Time |
|---|---|
| `cargo build` (debug, full) | 31.8 s |
| `cargo test -- --test-threads=1` | 5 m 02 s |
| `cargo build --release` (full) | **19 m 17 s** |
| `prove t/` on release | 2 m 52 s |
| `prove t/` on debug | 6 m 43 s |

The release compile is ~36× the debug compile and ~70 % of the whole target's wall-clock,
while running the TAP suite on debug instead of release costs only ~4 more minutes of
runtime. Worse, the release binary was never actually required here: CI does not invoke
`make test`, and its `test` job already runs `prove t/` on `target/debug/mutsu` (the
gc-stress and jit-stress jobs too), reserving the release build for `make roast`. So local
`make test` running `t/` on release was the outlier, and a *more lenient* one (release
runtime + a 60 s timeout vs CI's debug + 30 s).

`make test` now drops `cargo build --release` and runs `prove t/` on `target/debug/mutsu`
with `MUTSU_T_TIMEOUT=60`, cutting wall-clock from ~28 min to ~12 min and making local
pass/fail match CI's TAP step exactly. `make roast` is unchanged — it still builds and runs
on release, because the roast suite is timing-sensitive and long enough that release runtime
matters. The decision and measurements are recorded in
`docs/adr/0014-make-test-runs-tap-on-debug-binary.md`.
