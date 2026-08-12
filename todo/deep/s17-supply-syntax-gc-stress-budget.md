# S17-supply/syntax.t blows its gc-stress budget — interval/react wall-clock has regressed

`roast/S17-supply/syntax.t` is the top entry in the CI flake survey (7
failures, 6 of them on main pushes) and killed PR #6287's gc-stress job
(run 31549980182: exit 124 at 75/90, `Failed: 0`). Measured 2026-08-12 on
main (c42ec1e3b, release build, 12-core box):

- Plain (no GC env): PASS in ~78 s wall / 515 CPU-s. The 2026-07-25 note
  in `scripts/run-roast-test.sh` recorded 19-35 s for the same
  configuration, so even the plain run is ~2-4x slower now (same box
  class; background load differed, so treat as indicative).
- gc-stress env (`MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024
  MUTSU_GC_VERIFY=1`) with CI's `MUTSU_ROAST_TIMEOUT_SCALE=2` (240 s
  budget): **deterministic timeout at 70/90**, burning 2081 CPU-s at
  ~950% CPU. Reproduced 4/4 (3x at scale 1 = 120 s stopping at 68/90,
  1x at scale 2 = 240 s stopping at 70/90). Identical on the #6287
  branch — this is main behavior, not a PR regression.

The wall is at the interval-driven tests: test 70 (`No react guts crash
...` — 5x react over `Supply.interval(.001)` with `done` after 50 ticks)
and test 71 (`No races/crashes around interval that emits done` — 4
threads x 500 reacts each over `supply { whenever Supply.interval(0.001)
{ done } }`). Those 2000 short-lived reacts each poll a 1 ms interval;
under GC_VERIFY each tick's allocation churn is verified, which is where
the 950%-CPU spin goes.

Why it matters: CI's gc-stress job passes or fails this file on runner
load luck (PR #6289 passed the same code base minutes after #6287
failed). Every red costs a manual re-trigger and erodes trust in
gc-stress reds.

What to do (in preference order per docs/flaky-test-policy.md):

1. Root-cause the wall-clock regression since 2026-07-25 — candidates:
   the worker-pool ADR-0020 slices (#5921-#5926, thread churn →
   pooling changed interval scheduling), clone-slimming (#5928-#5934),
   or interval polling itself. `perf` the test-70/71 loop under the
   gc-stress env (`cargo build --profile profiling`).
2. If the cost is inherent to GC_VERIFY on 1 ms interval churn, give the
   file a bigger explicit budget in `scripts/run-roast-test.sh` (it
   already has 120 s; the gc-stress 2x makes 240 s — measured need on a
   loaded 12-core box is >240 s, so 240 base / 480 scaled) — with a
   fresh measurement note.
3. Only then consider `flaky-tests.txt` — but the policy bars
   quarantining an ununderstood mechanism, and this one is a
   deterministic-under-load budget miss, not non-determinism.

Repro:

```
cargo build --release
MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1 \
MUTSU_ROAST_TIMEOUT_SCALE=2 MUTSU_FUDGE=1 MUTSU_BIN=target/release/mutsu \
  prove -e 'scripts/run-roast-test.sh' roast/S17-supply/syntax.t
```
