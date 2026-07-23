# Flaky test policy

A test that fails without a code change costs more than the bug it might one
day catch: it forces a CI re-run, it trains everyone to re-run red CI instead of
reading it, and it hides the failures that are real. This document defines when
a test may be quarantined so it stops blocking a merge, what evidence that
requires, and how a quarantine is kept from becoming permanent.

## 1. Why this exists — the measured cost

`scripts/ci-flake-survey.sh 300` over the CI history ending 2026-07-23 (300 runs,
roughly two days of this repo's PR rate):

| | runs |
|---|---|
| CI runs surveyed | 300 |
| runs concluding `failure` | 31 |
| **failures whose every failing test was intermittent** | **13 (42%)** |
| of those, on `push: main` (a tree that had just passed CI on its PR) | 9 |

So four out of ten red builds were noise, and essentially every red build on
`main` was noise. That is the problem this policy addresses.

## 2. The evidence standard

"Flaky" is a claim about non-determinism. It must be shown, not asserted. Two
independent instruments:

**(a) Job spread — free, from CI history.** Every PR runs the same suite three
times: `test`, `gc-stress`, `jit-stress`. A genuine regression fails in all
three, because the code is broken in every configuration. A test that fails in
exactly ONE of the three, with the other two green on the same commit, is
non-deterministic by construction: same binary, same inputs, different verdict.
A failure on a `push: main` run is stronger still — main is protected, so that
exact tree passed the full suite minutes earlier on its PR.

`scripts/ci-flake-survey.sh [run-count]` mines this and prints a per-test tally
with `1-JOB` / `N-JOB` / `MAIN-PUSH` columns. High `1-JOB` with zero `N-JOB` is
the quarantine-candidate shape.

**(b) Repro rate — local, under emulated CI contention.**

```
cargo build --release
scripts/flake-repro.sh -n 20 -l 6 <test-file>            # default config
scripts/flake-repro.sh -n 20 -l 6 -e MUTSU_JIT=on -e MUTSU_JIT_THRESHOLD=2 <f>
scripts/flake-repro.sh -n 20 -l 6 -e MUTSU_GC=on -e MUTSU_GC_VERIFY=1 <f>
```

`-l` starts CPU burners so the timing pressure resembles `prove -j4` on a
4-core runner. The script reports `pass=N fail=M` and classifies the file as
`DETERMINISTIC PASS` / `FLAKY (M/N)` / `DETERMINISTIC FAIL`. It deliberately
bypasses the quarantine (`FLAKY_LIST=/dev/null`) so it measures the raw rate.

A `DETERMINISTIC FAIL` is a bug, not a flake, no matter what the CI history
suggested — go fix it.

## 3. What may be quarantined, and what may not

Quarantine only when the non-determinism is **understood** and **not a defect in
mutsu**:

- **Statistical by design.** The test samples a random process and asserts
  bounds that a correct implementation still violates with small probability
  (e.g. `roast/S02-types/baghash.t`'s Binomial(100, 1/3) `.roll` bounds). Roast
  is read-only, so the assertion cannot be tightened locally.
- **Wall-clock sensitivity that is not ours to fix.** The test asserts on real
  scheduling under `prove -j4` CPU contention, and the per-file timeout is
  already generous. Prefer raising the budget in
  `scripts/run-roast-test.sh:per_file_timeout` first — a bigger budget is a
  precise fix; a retry is a blunt one.
- **Harness cross-talk that has no clean fix.** Independent tests sharing
  process-global state. Prefer fixing the isolation (that is what
  `cargo test -- --test-threads=1` does for the GC collector's process-global
  `COLLECTING` flag) over quarantining.

Never quarantine:

- **A crash.** SEGV, `SIGABRT`, a Rust panic. An intermittent memory-safety bug
  is the most serious bug class in the tree; retrying it hides exactly the
  signal that would let us find it.
- **A wrong answer.** A concrete `not ok` from an assertion whose expected value
  is correct. `t/supply-live-grep-map-react-order.t` fails ~1 run in 20 with the
  events in the wrong order while `raku` produces the correct order 10/10 — that
  is a real race in mutsu's react drive loop, and quarantining it would have
  buried it.
- **Anything whose root cause is unknown.** "It passed on retry" is not a root
  cause. Investigate first; quarantine is a decision made after understanding,
  never instead of it.

This mirrors the triage protocol in CLAUDE.md ("Triaging a suspected-flaky
failure"), which has a track record: `t/wrap.t`, `t/placeholder.t` and
`t/tail-function.t` all sat mislabelled "flaky" for months and all three turned
out to be deterministic correctness bugs.

## 4. The mechanism

`flaky-tests.txt` is the quarantine ledger. One entry per line:

```
<test-path>  <added:YYYY-MM-DD>  <review:YYYY-MM-DD>  <reason>
```

Both test runners consult it through one retry engine:

- `scripts/run-roast-test.sh` (roast, already the `prove -e` target)
- `scripts/run-t-test.sh` (t/, new — so both suites behave identically)
- `scripts/flaky-retry.sh` (the engine)

For a listed file, a failed attempt's output is discarded and the file runs
again, up to `FLAKY_MAX_ATTEMPTS` (default 3). The first passing attempt is what
prove sees, preceded by a `# flaky-retry:` TAP comment. An unlisted file is
never retried — one attempt, streamed straight through.

Three properties worth stating explicitly:

1. **Quarantine is a re-roll, not immunity.** A deterministic failure inside a
   quarantined file fails all three attempts and still blocks the merge. What
   the retry absorbs is exactly the low-probability event.
2. **Quarantine is per file, not per subtest.** `baghash.t` is quarantined for
   four `.roll` subtests, and a regression in its other 340 subtests would be
   re-rolled too — but only if it were *also* intermittent. A deterministic one
   still fails.
3. **Retries are never silent.** Every retry is appended to
   `tmp/flaky-retries.log`, and CI turns that into a `::warning::` annotation
   and a job-summary table. A quarantined test whose retry count climbs is
   getting worse, and that has to be visible.

## 5. Keeping the ledger honest

Every entry carries a `review` date, at most 90 days out. `make
check-flaky-list` (run in CI) fails when an entry is past it. Renewing an entry
means re-running `scripts/ci-flake-survey.sh` and `scripts/flake-repro.sh` and
recording the new numbers in the reason — not bumping the date.

An entry is removed when either the root cause is fixed, or a survey shows it
has stopped appearing. Removal needs no ceremony: delete the line. If it comes
back, the survey will show it.

## 6. Prose lists are not a mechanism

CLAUDE.md's "Known flaky tests" section stays as *context* — it explains the
mechanism behind each historical flake and records the de-flaked ones, which is
genuinely useful. But it is not, and must not become, the thing CI consults.
The 2026-07-23 survey found that section had drifted from reality in both
directions: it listed `roast/S02-types/mixhash.t` and a blanket "`S17-*`
concurrency tests", while the tests that actually cost re-runs
(`roast/S17-promise/nonblocking-await.t`, `t/supply-on-demand-closing-tap.t`,
the `gc::gc_ptr` unit test) were not in it at all. A machine-readable ledger
with review dates cannot drift that way without CI saying so.
