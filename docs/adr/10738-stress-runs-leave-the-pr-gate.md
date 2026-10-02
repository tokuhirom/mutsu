# ADR-10738: The GC/JIT stress runs leave the PR gate; the debug-binary TAP pass stays as `debug-tap`

- **Status**: Accepted (user decision 2026-10-02). Supersedes the parts of three ADRs that put the
  stress runs on the PR gate:
  [ADR-0003](0003-default-on-gc-trigger.md) §2 gate (a), "gc-stress roast made blocking and kept
  green"; [ADR-0004](0004-jit-strategy.md) §2.6, "a `jit-stress` job in CI from J2 onward"; and
  the [ADR-0075](0075-make-test-runs-tap-on-release-binary.md) §2 clause "`gc-stress` and
  `jit-stress` keep running `t/` on the debug binary", whose role passes to `debug-tap`.
- **Date**: 2026-10-02
- **Deciders**: tokuhirom, Claude
- **Issue**: [#10738](https://github.com/tokuhirom/mutsu/issues/10738)
- **Related**: `docs/ci-pipeline.md`, `docs/flaky-test-policy.md`,
  `news/2026-09/ci-builds-the-release-binary-once.md`,
  `news/2026-09/ci-stress-jobs-stop-paying-for-a-serial-tap-run.md`

## Context

`ci.yml` ran 13 jobs on every PR and every `push: main`, about 65 runner-minutes per run. On run
36952232722 the required `test` check took **39m24s** of wall clock. Only about 14 minutes of that
was compute on the critical path (`changes` → `build` → `test-suites` → `test`). The other ~25
minutes (63%) were **runner queue wait**: `build` waited 10m47s for a runner, `test-suites` 8m50s,
and the 3-second `test` aggregator 4m35s. This repository lands many agent PRs an hour, and every
PR run and every `push: main` run compete for the same concurrency slots. So the lever on latency
is runner-minutes and job count, not the speed of any single test.

Four of the 13 jobs were stress runs: `gc-stress-tap`, `gc-stress-roast`, `jit-stress-tap` and
`jit-stress-roast`, plus their `gc-stress` / `jit-stress` aggregators. Together they used about
31 of the 65 runner-minutes:

- **gc-stress** ran with `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`: `cargo test`,
  then `prove t/` on a debug binary, then the roast whitelist on release.
- **jit-stress** ran with `MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2`: `prove t/` on debug, then roast on
  release.

These jobs were built while the cycle collector (ADR-0003) and the JIT (ADR-0004) were being
brought up. Both have since shipped and are default on, so the ordinary jobs already exercise the
default GC trigger and the default JIT threshold.

### Measured yield (2026-09-11 .. 2026-10-02)

There were about 2500 `ci.yml` runs and 148 of them failed:

| Failed jobs | Runs |
| --- | --- |
| Stress jobs only | 28 |
| Stress and non-stress jobs | 43 |
| Non-stress jobs only | 77 |

None of the 28 stress-only failures was a `VERIFY FAIL`, a JIT output divergence or a
`debug_assert!`. 27 were timing-sensitive concurrency tests that timed out or crashed under the
slower configuration. The remaining one was an infra failure (a mold download). The concurrency
failures came from four files:

- `t/concurrency/concurrent-lane-decline-routes.t`: 9 runs on main
- `t/concurrency/thread-lock/closure-self-capture-call-arg-cross-thread.t`: 8 runs on 09-26
- `t/concurrency/thread-lock/scheduler-cue-dispatch-oracle.t`
- `roast/S17-procasync/kill.t`

Every PR branch involved went green on a later run. In other words, over three weeks the stress
gate's unique catches were flakes that blocked unrelated PRs.

### What the debug-binary TAP runs were also doing

The two TAP halves were also the **only** place the `debug_assert!`s in `src/` ran across the
whole `t/` suite: ADR-0075 moved the PR's own TAP step to the release binary, where those asserts
compile away. Several recent invariants are deliberately checked only that way, rather than by a
focused test:

- `put_param_slot`'s slot index
- the env-lookup "never held" latch
- the binder's baked parameter symbols
- the `(kind, method)` table audited against the full dispatch walk
- `FunctionTable::audit`
- the `GetLocal` fast-path contract

That coverage is independent of the stress knobs, and it is worth keeping on every PR.

### Why the unit tests ran GC-off

The crate's own test build defaults the collector **off** (`gc_enabled` in `src/gc/gc_ptr.rs`:
`None => !cfg!(test)`). The reason is that parallel `cargo test` threads cross-talk through the
process-global collector state. CI and `make test`, however, already run
`cargo test -- --test-threads=1`, which removes that reason. Until this change, only
`gc-stress-tap` ran the unit tests GC-on.

## Decision

1. **PR gate (`ci.yml`)**:
   - Replace `gc-stress-tap` and `jit-stress-tap` with a single **`debug-tap`** job. It runs
     `prove t/` on a debug binary with **default** runtime settings, and runs alongside `build`.
   - Drop `gc-stress-roast`, `jit-stress-roast` and the `gc-stress` / `jit-stress` aggregators.
   - `test` now aggregates `build`, `test-check`, `test-suites` and `debug-tap`.
2. **Unit tests run GC-on.** `test-check` and `make test` set `MUTSU_GC=on` on their serialized
   `cargo test`. The `cfg!(test)` default stays off, so an ad-hoc parallel `cargo test` remains
   safe.
3. **Stress runs move to `.github/workflows/stress.yml`**, unchanged in configuration and content
   (`build` + the four stress jobs).
   - It runs **nightly on `main`** (18:41 UTC). A failed nightly run opens, or comments on, the
     open issue labelled `ci:stress`.
   - It also runs **on demand** (`workflow_dispatch`, any branch). A PR that changes the cycle
     collector, the JIT or the concurrency runtime should dispatch it before merging and link the
     run.
4. **Required checks** (the repository ruleset for `main`): `gc-stress` and `jit-stress` are no
   longer required. The required checks are `test`, `wasm-e2e`, `lint-configs`, `miri` and
   `changes`.

## Consequences

- **Cost**: a PR run drops from 13 jobs and ~65 runner-minutes to 9 jobs and ~43 runner-minutes,
  and the two aggregators that each waited 4–6 minutes for a runner are gone.
- **Flakes**: the concurrency tests that flaked under the stress configuration no longer block
  unrelated PRs. They surface nightly instead, where somebody has to own them.
- **Accepted risk**: a GC soundness or JIT codegen regression that only the stress knobs expose
  now lands on main and is caught within a day, instead of being blocked at its PR. The measured
  window above contained no such regression. The on-demand dispatch is the mitigation for changes
  that target exactly that machinery.
- **Flake triage loses one instrument**: the three-configuration "job spread" in
  `docs/flaky-test-policy.md` (default / GC / JIT on every PR) shrinks to two jobs for `t/`
  (release and debug) and one for roast. The nightly run is the third signal.
- **Reversible**: if nightly stress failures turn out to be real regressions often enough to cost
  more than the PR-side wait, move the jobs back. Nothing else changes shape.

## Rejected alternatives

- **Keep the stress jobs, only cut the aggregators.** This saves the aggregators' queue wait but
  none of the ~31 runner-minutes, and it keeps the stress-only flakes on the PR path.
- **Fold the debug TAP run into `test-check`.** `cargo test` already leaves a debug binary there,
  so no extra compile would be needed. But it would stretch that job to ~14 minutes, matching the
  critical path, and a red check would no longer say whether a unit test or the suite broke.
- **Combine both stress configurations into `debug-tap`.** This would keep stress coverage at
  near-zero cost, but it puts the very tests that flake under stress back on the PR gate. That is
  the cost this ADR removes.
