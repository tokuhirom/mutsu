# The GC/JIT stress runs leave the PR gate

Every PR used to run the whole suite three times: in the default configuration,
under GC stress (`MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`) and under
JIT stress (`MUTSU_JIT_THRESHOLD=2`). The four stress jobs used about 31 of each
CI run's ~65 runner-minutes.

The cost showed up as waiting more than as compute. On a typical run the
required `test` check took 39 minutes. Only about 14 of those were compute on
the critical path. The remaining ~25 were queue time, because every agent PR and
every `push: main` run competes for the same runner slots.

From 2026-09-11 to 10-02 the stress jobs failed a PR on their own 28 times. None
of those failures was a GC VERIFY violation, a JIT divergence or a
`debug_assert!`. 27 were timing-sensitive concurrency tests that timed out under
the slower configuration, and one was an infra failure.

## What changed

- **Stress runs.** They moved to a new workflow, `.github/workflows/stress.yml`,
  with the same jobs and the same knobs. It runs nightly on `main` and on demand
  on any branch. A red nightly run opens, or comments on, the open `ci:stress`
  issue.
- **`debug-tap`.** The stress TAP halves were also the only place the
  `debug_assert!`s ran across the whole `t/` suite. That role is kept: the new
  `debug-tap` job in `ci.yml` runs `prove t/` on a debug binary with default
  settings.
- **Unit tests with the collector on.** The unit tests now run with
  `MUTSU_GC=on`, in `test-check` and in `make test`. The test build defaulted
  the collector off only because parallel test threads cross-talk through its
  global state, and these runs are serialized anyway.
- **Smaller PR runs.** A PR run is now 9 jobs instead of 13, at about 43
  runner-minutes.

The decision and the measurements behind it are recorded in ADR-10738.
