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
times, in three configurations: default, GC on, JIT hot. Since the CI jobs were
split into halves, the three jobs running a given file are `test-suites` /
`gc-stress-tap` / `jit-stress-tap` for a `t/` file and `test-suites` /
`gc-stress-roast` / `jit-stress-roast` for a roast file (`test`, `gc-stress` and
`jit-stress` are now aggregator jobs that run no tests). A genuine regression
fails in all three, because the code is broken in every configuration. A test
that fails in exactly ONE of the three, with the other two green on the same
commit, is non-deterministic by construction: same binary, same inputs,
different verdict.
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

This mirrors the triage protocol in §8 below, which has a track record: `t/wrap.t`, `t/placeholder.t` and
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
4. **A signal death is never retried, even inside a quarantined file.**
   `flaky-retry.sh` checks the exit code of every attempt: a process killed by
   a signal (SIGSEGV, SIGABRT, ...) exits with `rc = 128 + signum`, so `rc >=
   128` fails the run immediately, with a `died of signal N -- NOT retried`
   comment, instead of re-rolling. This enforces §3's "never quarantine a
   crash" mechanically rather than relying on nobody ever listing a crashing
   file. Before this check existed, `roast/integration/advent2014-day05.t`
   (already quarantined for an unrelated timing reason) aborted with SIGABRT
   from heap corruption, was silently retried, and the retry happened to pass
   — a genuine memory-safety bug reached a green CI run undetected
   (`todo/deep/procasync-stress-segv.md`). A companion allowlist in
   `scripts/report-crash-reports.sh` catches the same class of bug even
   *outside* a quarantined file's own process: that script now fails the job
   whenever a crash report's `argv:` is not on a short list of known,
   deliberately-provoked crashes (e.g. `roast/S29-os/system.t`'s
   `NativeCall`/`strdup(0)` probe), and it now runs on every job (`if:
   always()` in `ci.yml`) rather than only on an already-failing one — a
   deliberately-crashed *subprocess* a test asserts on can otherwise leave the
   overall job green while an unrelated genuine crash goes unnoticed the same
   way.

## 5. Keeping the ledger honest

Every entry carries a `review` date, at most 90 days out. `make
check-flaky-list` (run in CI) fails when an entry is past it. Renewing an entry
means re-running `scripts/ci-flake-survey.sh` and `scripts/flake-repro.sh` and
recording the new numbers in the reason — not bumping the date.

An entry is removed when either the root cause is fixed, or a survey shows it
has stopped appearing. Removal needs no ceremony: delete the line. If it comes
back, the survey will show it.

## 6. Prose lists are not a mechanism

§7 below (formerly AGENTS.md's "Known flaky tests" section) stays as *context* — it explains the
mechanism behind each historical flake and records the de-flaked ones, which is
genuinely useful. But it is not, and must not become, the thing CI consults.
The 2026-07-23 survey found that section had drifted from reality in both
directions: it listed `roast/S02-types/mixhash.t` and a blanket "`S17-*`
concurrency tests", while the tests that actually cost re-runs
(`roast/S17-promise/nonblocking-await.t`, `t/supply-on-demand-closing-tap.t`,
the `gc::gc_ptr` unit test) were not in it at all. A machine-readable ledger
with review dates cannot drift that way without CI saying so.

## 7. Flake history — context, not the ledger

The prose list here is *context* (why each historical flake happened, and the de-flaked ones), NOT the thing CI consults — that is `flaky-tests.txt`. Some tests are genuinely non-deterministic (concurrency/timing/CI-load sensitive) and fail intermittently. When a `make roast` / `make test` failure is **only** in the list below and your change is unrelated (e.g. an operator/parser fix), treat it as flaky: re-run the single file a few times before assuming a regression. Do **not** remove it from the whitelist.

- `S17-*` concurrency tests — may fail occasionally under heavy parallel load, pass on retry. (A 2026-07-05 audit ran all 97 whitelisted S17 files ×7 `-j4` release sweeps: the only repeat offender was `batch.t`, root-caused and fixed — see below.)

**De-flaked (do NOT treat a failure here as flaky — it's a regression):**

- `roast/S02-types/bag.t`, `roast/S02-types/baghash.t`, `roast/S02-types/mixhash.t` — the Binomial(100, 1/3) `.roll` bounds that made these three statistically flaky by design were **fixed upstream**: the 2026-09-11 roast re-vendor (commit `85a8790`, upstream "Make more statistical tests less likely to fail") raises every sample from 100 to 100000 rolls and scales the bounds with it, putting the assertions ~100 standard deviations from the mean. All three were dropped from `flaky-tests.txt` in that PR (3/3 green locally, ~1-2s each on release). A failure here is real again.
- `roast/S04-exceptions/exceptions-alternatives.t` — the 2026-07-15 "occasional jit-stress timeout" (exit 124, "planned 3 ran 2") was NOT load noise: subtest 3's `JSON::Tiny::Grammar.parse` of the subprocess's JSON stderr took ~12.6s (raku: 2ms) because ratcheted separated quantifiers (`rule pairlist { <pair> * % \, }`) backtracked exponentially, leaving only a slim margin under the 30s budget. Fixed the same day: ratcheted `* %` is possessive now (Rakudo semantics) and the whole file runs in ~1s. Pin: `t/regex-sep-quantifier-ratchet.t`. A timeout here is real again.
- `t/lock.t` "Lock::Async protects shared array pushes" — was a real lost-update race (listop `push` inside `protect` wrote the base shared_vars key, which a parent-thread stale env sync clobbered wholesale). Fixed in #4167 by routing all plain-lexical shared-array pushes through the `__mutsu_atomic_arr::` store.
- `roast/S17-supply/batch.t` "we can batch by time and elems" — was a deterministic logic bug, not load flakiness: `batch(:seconds)` anchored its time window to tap-registration `Instant` instead of absolute `time div $seconds` periods, firing a spurious 1-element flush when the tap was registered just after a period boundary. Pin: `t/supply-batch-period.t` (forces the boundary alignment).
- `roast/S02-names-vars/perl.t` — the historical "typed-container alloc/hash-order" mid-run abort no longer reproduces (2026-07-05: 72 clean runs, debug+release, under 12× CPU contention); re-whitelisted.
- `roast/S02-types/hash.t`, `roast/S09-typed-arrays/hashes.t` — the "CI-load-sensitive timeout" label is stale: both complete in ~0.3s on a release build now. A failure here is real — see triage below.
- `t/io-socket-recv-limit.t` — the "fails under `-j4` load" label was wrong: it was a deterministic **port collision**. `IO::Socket::Async.listen(host, 0)` did not let the OS assign an ephemeral port; it substituted one from a process-local counter seeded identically in every process, so concurrent mutsu processes all asked for the same port and whichever bound second died. On top of that, this file and `t/io-socket-async-bin.t` both hardcoded 19995 (serial: 5/5 PASS, `-j2`: 5/5 FAIL). Fixed in #4512 — port 0 now reaches `bind()`, and the test asks the tap for the port it got. **Never hardcode a port in a new test**: listen on 0 and read `.socket-port`.

`make roast` removes `temp-file-RT-126006-test` before starting: a stale copy
left by an interrupted `roast/S32-io/spurt.t` would otherwise make that test
abort with "cannot run test while file ... exists".

## 8. Triaging a suspected-flaky failure — don't mislabel a real bug

"Flaky" is a claim about *non-determinism*; verify it before trusting it. A failure that reproduces every run is a real bug to fix, not noise to skip. `t/wrap.t`, `t/placeholder.t`, and `t/tail-function.t` sat here for months labeled "flaky / pre-existing" when all three were **deterministic correctness bugs** (closure-capture env writeback, scope-lost Seq iterator, missing `%_` placeholder capture — fixed in #2629 / #2630 / #2632). Before adding or trusting a flaky label:

1. **Re-run the single file ~5× in a release build** (`cargo build --release && prove -e target/release/mutsu <file>`). Fails every time → deterministic → fix it, don't skip it.
2. **Read the failure shape.** A *timeout* / `exit 255` with `Failed: 0` (bad plan, ran fewer than planned) is plausibly load/timing. A *concrete subtest* failure (`Failed: N`, a real `not ok` assertion) is almost always a logic bug — even on a "known flaky" file. Investigate the subtest.
   - **BUT `exit 255` + `Failed: 0` is NOT automatically flaky.** "Ran N of M, Failed: 0" also happens when your *own* change throws an unexpected exception **mid-file** (e.g. a false-positive `X::Redeclaration` / `X::Assignment::RO`), which aborts the rest of the file with `Runtime error: Test failures` — looking exactly like a timeout. **The tell:** it reproduces *deterministically* on your branch but NOT on `main`, and the `(N+1)`th test is precisely the construct your change touches. Before declaring flaky, run the exact file on your branch vs `main` (`prove -e target/debug/mutsu <file>`); if your branch aborts and `main` completes, it is a real regression you introduced — fix it, do not re-trigger CI. (Seen in Tier-2: a sigil-blind `constant` redeclaration check aborted `S06-operator-overloading/sub.t`; a literal-LHS `s///` RO check aborted `S05-metasyntax/regex.t` on `TR///`.)
3. **debug vs release.** A debug-only timeout on a heavy test can be load; a failure that reproduces in *release* is real (CI uses release).
4. Only label flaky if it actually **passes on retry**; note the pass/fail ratio when you do.

The `t/` TAP suite is **fatal** in CI (`prove ... t/`, no `|| echo` fallback) — a deterministic `t/` failure fails the CI job, same as roast.
