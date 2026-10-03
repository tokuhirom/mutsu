# ADR-11581: CI spends the runner budget on the PR gate; `main`, Pages and wall-clock bench run on a schedule, and the release runs everything

- **Status**: Accepted (user decision 2026-10-03). Supersedes the
  [ADR-10738](10738-stress-runs-leave-the-pr-gate.md) clause that `debug-tap` stays on the PR gate;
  its role (the suite-wide `debug_assert!` pass) is unchanged, only where it runs.
- **Date**: 2026-10-03
- **Deciders**: tokuhirom, Claude
- **Issue**: [#11581](https://github.com/tokuhirom/mutsu/issues/11581)
- **Related**: `docs/ci-pipeline.md` ("Runner budget"), `.github/workflows/{ci,bench,pages,release}.yml`,
  `news/2026-10/main-ci-and-pages-on-a-schedule.md`

## Context

The repository is public, so GitHub Actions minutes are free. What is scarce is **concurrency**:
the free plan runs at most 20 hosted jobs at once, and that pool is shared by every PR run, every
post-merge run on `main`, Bench, Pages, the nightly stress runs and the ecosystem sweep.

Measured on 2026-10-03, after ADR-10738 had already moved the stress runs off the PR gate:

- A PR run (`ci.yml`) was 7 jobs and ~40 runner-minutes. Run 37125005503 did ~25 minutes of work
  in ~33 minutes of wall clock: `build` waited 7 min for a runner, `test-check` 10 min,
  `wasm-e2e` 14 min, the 2-second `test` aggregator 3 min.
- 54 CI runs were created in a 43-minute window (≈75/hour). 20 runners at ~40 runner-minutes a
  run serve about 30 runs an hour. Demand was roughly twice capacity.
- On top of PR runs, every merge (~100 a day) started a full `ci.yml` run on `main`, a Bench run
  (~30 min: wall-clock suite + callgrind), and up to two Pages deploys (one per push, one per Bench
  completion). Because of pending-run replacement, about one CI run and one Bench run were in
  flight on `main` at all times.

Of the PR run's ~40 runner-minutes, the two largest jobs that do not decide whether the change is
*correct on the host* were:

- `wasm-e2e` (~10 min, 9 of them `wasm-pack` building the browser package), which runs the
  browser/Node behaviour of the wasm build. Whether the wasm32 build *compiles* is already checked
  on every PR by `lint-configs` (clippy for wasm32).
- `debug-tap` (~8 min), the suite-wide `prove t/` on a debug binary, which exists for the
  `debug_assert!`s in `src/`. Every PR's `test-check` still runs `cargo test` with debug
  assertions.

## Decision

1. **The PR gate keeps** `changes`, `build`, `test-check`, `test-suites`, `lint-configs`, `miri`
   (when GC/value code changes) and the `test` aggregator.
2. **`wasm-e2e` and `debug-tap` are post-merge only.** They run on every non-PR run of `ci.yml`
   (hourly on `main`, `workflow_dispatch`, and the release gate) and are skipped on pull requests.
   `wasm-e2e` stays a job (skipped) because it is a required check name in the `main` ruleset and a
   skipped required check counts as success; the `test` aggregator accepts a skipped `debug-tap`
   only on a pull request.
3. **CI on `main` runs hourly**, not per push. The `changes` job takes the head of the last green
   non-PR run on `main` as the diff base: unchanged `main` skips every build job, and a stretch of
   documentation-only merges skips them like a docs-only PR. These runs still save the cargo caches
   PRs restore.
4. **Every release runs the whole of `ci.yml`.** `ci.yml` accepts `workflow_call` with
   `full: true`, which forces every job on (including Miri, `wasm-e2e` and `debug-tap`)
   regardless of classification; `release.yml` calls it on the tagged commit, and the npm publish
   and the GitHub Release both `need` it. So nothing ships that the post-merge-only jobs have not
   passed on exactly that commit.
5. **Bench splits its two series by cadence.** The deterministic series (callgrind instruction and
   heap-allocation counts; ~0.1% run-to-run spread) stays per push to `main`, so a step is
   attributable to one merge. The wall-clock series (16-33% commit-to-commit noise) runs hourly and
   skips a commit it already has. Each has its own concurrency group so merge traffic cannot starve
   the hourly run.
6. **Pages deploys every two hours**, plus after a tagged Release and after the daily Ecosystem
   sweep, and on `workflow_dispatch`.

## Consequences

- PR runs drop from ~40 to ~22 runner-minutes and from 7 to 5 runner-occupying jobs, and the
  post-merge load drops from ~1 full CI run + 1 Bench run continuously to about one CI run an hour
  and the callgrind half of Bench. Together this roughly doubles PR throughput on the same 20
  runners.
- **A wasm runtime regression or a `debug_assert!` that only `prove t/` reaches is found after
  merge**, by the next hourly run on `main`, instead of on its PR. That run can cover several
  merges, so bisect over the PRs merged in that hour. The release gate guarantees neither can ship.
  A PR that changes wasm-specific code (`src/wasm*`, `site/`, the npm package scripts) or the
  debug-assertion paths should run `ci.yml` on its branch by `workflow_dispatch` before merging.
- A red run on `main` may be two PRs that are each green alone; `docs/flaky-test-policy.md` and
  `scripts/ci-flake-survey.sh` count any non-PR run on `main` as the post-merge signal.
- The site (roast figure, bench trend) lags `main` by up to two hours; the wall-clock bench series
  has about one point an hour instead of one per merge it managed to measure.
- A release takes longer (it now runs Miri, `wasm-e2e` and the whole suite before publishing).

## Alternatives considered

- **Self-hosted runner on the maintainer's 12-core box.** The largest capacity gain, but on a public
  repository it lets PR code execute on a personal machine; that weakens a security boundary and
  needs its own decision (AGENTS.md, "Never weaken a security protection").
- **Paid larger runners.** Not free.
- **A merge queue with batching.** Rejected earlier as impractical at ~100 merges a day (AGENTS.md,
  "A new gate is watched after it lands").
- **Merging `debug-tap` into `test-check`.** Saves one job slot and setup, but `debug-tap` builds at
  `opt-level=1` and `test-check` at 0, so the two builds share little; the gain is ~2-3 runner-minutes
  against ~8 for moving the job post-merge.

## Amendment (2026-10-03): `build` folds into `test-suites`

`ci.yml`'s `build` job compiled the release binary and handed it to the other jobs as an artifact.
That paid off while three jobs consumed it (`test-suites`, `gc-stress`, `jit-stress`). After
ADR-10738 moved the stress jobs to `stress.yml`, `test-suites` was the only consumer, and it already
waited for `build` (`needs: build`). So the split bought no parallelism. It cost a second runner
slot per run, the artifact upload and download, and the queue wait between the two jobs: 7.5
minutes on run 37125005503 (`build` finished 13:18:30, `test-suites` started 13:26:00).

`test-suites` now builds the binary itself (same toolchain, `ci-linux-release` rust-cache key, mold,
`CARGO_PROFILE_RELEASE_DEBUG=false`) and runs the suites on it. A compile failure shows as a red
`test-suites` at its "Build (release)" step. The `test` aggregator no longer lists `build`; `build`
was never a required check. `stress.yml` keeps its own shared build job, because it has two
consumers.

## Outcome (first hours, 2026-10-03)

- PR runs went from ~33 to 14-19 minutes of wall clock, and jobs on the 18:43 run on `main`
  started within seconds of being queued.
- GitHub's `schedule` trigger fired far less often than its cron asked for. Between the merge at
  14:34 and 21:42 UTC, the hourly CI and Bench crons each fired once, and the two-hourly Pages cron
  fired once. The existing three-hourly `claim-label` cron shows the same 4-6 h spacing. In
  practice, the post-merge cadence is best-effort, roughly every 4-5 hours. The run that did fire
  worked as designed: diff base resolved, `wasm-e2e`, `debug-tap` and Miri green, caches saved.
