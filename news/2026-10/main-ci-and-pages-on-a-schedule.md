# CI spends its runners on the PR gate; `main`, Pages and wall-clock bench move to a schedule

The repository is public, so GitHub Actions minutes are free, but the free
plan caps hosted runners at 20 concurrent jobs. At ~100 merges a day PR runs
were queueing 5-15 minutes per job: a PR whose jobs did ~25 minutes of work
took ~33 minutes of wall clock, and a 2-second aggregator job waited 3 minutes
for a runner. About 75 CI runs an hour were being created against a capacity
of about 30.

[ADR-11581](../../docs/adr/11581-ci-runner-budget.md) rebalances the budget:

- **PR runs drop from ~40 to ~22 runner-minutes.** `wasm-e2e` (~10 min) and
  `debug-tap` (~8 min) no longer run on pull requests; they run on `main` and
  in the release gate. The wasm32 build is still clippy-checked on every PR.
- **CI on `main` runs hourly** instead of per merge, diffing against the last
  green run on `main` so an unchanged or docs-only `main` skips the build.
- **Every release runs the whole of `ci.yml`** on the tagged commit
  (`workflow_call`, `full: true`), and npm / the GitHub Release wait for it.
- **Bench** keeps the deterministic callgrind series per push and measures the
  noisy wall-clock series hourly.
- **Pages** deploys every two hours, plus after a Release and after the daily
  Ecosystem sweep.
