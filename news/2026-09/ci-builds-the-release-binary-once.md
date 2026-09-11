# CI builds the release binary once, and every job says what it tests

`cargo build --release` ran three times on every commit — in `test`, in
`gc-stress` and in `jit-stress` — producing a byte-identical binary each time.
It is byte-identical because `MUTSU_GC` and `MUTSU_JIT` are runtime environment
variables rather than cargo features, which is the same fact that already let
those three jobs share one `ci-linux` rust-cache key. Measured on run
34582469117 the three builds cost 4m05s + 3m04s + 4m20s — 11m29s of runner time
per CI run, buying nothing.

Each stress job also ran two unrelated things back to back: the TAP suite on a
debug binary, then the roast whitelist on a release one. That made the job as
long as the sum of its halves instead of the longer of the two, and collapsed
both into a single opaque red `gc-stress` when either failed.

## What changed

A `build` job now compiles the release binary once and uploads it as an
artifact, the way `ecosystem-sweep.yml` already builds one binary for its 27
shards. The jobs that only *run* the interpreter download it and install no
toolchain, no cargo cache and no linker at all:

```
build ──┬─> test-suites ─────┬─> test        (required)
        │   test-check ──────┘
        ├─> gc-stress-roast ─┬─> gc-stress   (required)
        │   gc-stress-tap ───┘
        └─> jit-stress-roast ┬─> jit-stress
            jit-stress-tap ──┘
```

Each heavyweight unit is split at the seam between "needs a compiler" and
"needs a suite". `test-check` keeps fmt, clippy, the unit tests, the
language-server crate and the three ledger validators (now first, so a
mis-sorted whitelist fails in seconds rather than after a clippy pass);
`test-suites` keeps the TAP suite, both roast passes and the bundled-library
gate. `gc-stress-tap` keeps `cargo test` and `prove t/` on a debug binary —
that is still where the `debug_assert!`s get their suite-wide pass — while
`gc-stress-roast` runs the whitelist on the shared release binary. Same for
jit. Neither `test-check` nor the TAP halves wait for `build`; they run beside
it.

## Keeping the required checks

Branch protection's required status checks are **job names**: `test`,
`wasm-e2e`, `gc-stress`. Renaming a required job does not fail a PR — it leaves
the check uncreated, which leaves the PR pending forever and unmergeable, with
no error anywhere. So `test`, `gc-stress` and `jit-stress` survive as
aggregators that run no tests and exist for their names. Each runs
`if: always()` (a skipped required check counts as *success*, so the aggregator
must not be skippable) and fails unless every job it depends on reports
`success`. `build` is in each aggregator's `needs` for the same reason: a half
that never ran because the build failed reports `skipped`, and without the
build's own result that is indistinguishable from the documentation-only case —
which the aggregator recognises explicitly, from the `changes` job's output,
rather than inferring from a skip.

## Second-order details

- Artifacts are zips and carry no executable bit, so every consumer re-applies
  it and runs `--version` as a two-second proof. The download path is
  `target/release` and not a scratch directory because bundled batteries are
  resolved relative to the executable (`target/<profile>/../../modules`).
- The site-snippets step defaulted to `target/debug/mutsu` and only ever worked
  because the `cargo test` step earlier in the same job left a debug binary
  behind. It now names `target/release/mutsu` explicitly.
- `gc-stress`'s "No GC verify violations" step grepped both stress logs at the
  end of one job. Each half now greps its own, and a missing log is a failure
  rather than a silent pass.
- The rust-cache key split in two. While every job compiled both profiles, one
  entry held both; `build` now compiles only release and the three debug jobs
  only debug, so a single shared key would make the save a race whose winner
  decides which profile everyone else recompiles from scratch. `build` owns
  `ci-linux-release`; the debug jobs still share `ci-linux`.
- `scripts/ci-flake-survey.sh` counts how many jobs of a run saw a given test
  fail, which is the repo's flakiness instrument. The count still spans three
  configurations — the job names are just `test-suites` / `gc-stress-tap` /
  `jit-stress-tap` for a `t/` file now. It skips the aggregator jobs, which
  would otherwise add a phantom row to every genuine failure.

## What this does not fix

The wall-clock win is modest and hands the critical path to `wasm-e2e`
(12m56s, of which `Build npm package` is 8m54s). The real payoffs are runner
minutes and a red check that names which half failed. Anything past that is a
question about `wasm-e2e`, not about these jobs.

Closes #7982.
