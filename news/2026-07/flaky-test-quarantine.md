# An evidence-based flaky-test quarantine, so noise stops blocking merges

CI re-runs were being forced too often by tests that fail without a code change.
Mining the GitHub Actions history with a new `scripts/ci-flake-survey.sh` over
the 300 CI runs ending 2026-07-23 quantified it: of 31 red builds, **13 (42%)
had no failing test that wasn't intermittent**, and 9 of those were on
`push: main` — a tree that had passed the full suite minutes earlier on its PR.
So four in ten red builds were noise, and essentially every red build on `main`
was noise.

## The survey also caught two real bugs and one mislabelled folklore

The job-spread signal (a test that fails in one of the three CI job
configurations while the other two pass the same commit is non-deterministic by
construction) plus a local repro harness (`scripts/flake-repro.sh`, which runs a
file N times under emulated `-j4` CPU contention) separated genuine flakes from
deterministic failures hiding under a "flaky" reputation:

- `roast/S17-promise/nonblocking-await.t` test 28 and
  `t/supply-live-grep-map-react-order.t` test 5 both fail intermittently while
  `raku` runs them 10/10 correct — they are **real concurrency races** in
  mutsu's `await`-of-nested-iterables and react-drive-loop ordering. They are
  now recorded as PLAN.md §8.19 to fix, not quarantined.
- The `CLAUDE.md` "Known flaky tests" prose had drifted from reality in both
  directions: it listed tests that no longer flake and a blanket "`S17-*`", while
  the tests actually costing re-runs weren't in it at all.

That drift is the core lesson: a hand-maintained prose list is not a mechanism.

## The mechanism

- **`flaky-tests.txt`** — a machine-readable quarantine ledger. Each entry is a
  path, an added date, a review date, and a reason citing measured numbers.
- **`scripts/flaky-retry.sh`** — one retry engine both test runners go through
  (`scripts/run-roast-test.sh` and the new `scripts/run-t-test.sh`). A listed
  file that fails is re-rolled up to three times; an unlisted file runs once.
  Crucially, quarantine is a re-roll, **not immunity**: a deterministic failure
  fails all three attempts and still blocks the merge. Every retry is logged and
  surfaced in the CI job summary, so a quarantine that is silently getting worse
  becomes visible.
- **`scripts/check-flaky-list.sh`** (`make check-flaky-list`, gated in CI) —
  validates the ledger and, most importantly, **fails once an entry's review
  date passes**, forcing a re-measurement instead of letting a quarantine become
  permanent.
- **`docs/flaky-test-policy.md`** — the evidence standard and the bar for adding
  an entry: the non-determinism must be understood and must not be a defect in
  mutsu. Crashes, wrong answers, and unknown root causes are never quarantined.

Only two tests meet that bar today — `roast/S02-types/baghash.t` and
`mixhash.t`, whose `.roll` subtests bound a Binomial(100, 1/3) sample that a
correct RNG still violates with small probability, on read-only roast files that
cannot be tightened. Everything else the survey flagged was either a real bug or
already fixed.

As a related hardening, the default-config `cargo test` now runs with
`--test-threads=1` (matching the gc-stress job, which already did): the GC
collector's `COLLECTING` flag is process-global, and a collect on one test
thread was intermittently tripping a `debug_assert!` in an unrelated test — the
cause of the `gc::gc_ptr::tests::arc_and_gc_strong_counts_stay_in_lockstep` red
main builds.
