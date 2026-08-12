# t/supply-done-in-tap-callback-is-not-a-failure.t test 3 is load-flaky on main

Measured 2026-08-12 during the Text::CSV campaign (session running the
for-loop param-restore fix): under 24-way parallel invocation of the same
file on a debug build, **test 3 fails 10/24 runs** on a binary built from
main (commit 09ab95642, pre-change control build), and it failed once in an
ordinary `make test` (Files=3064). Serial re-runs pass 5/5, so this is
load/timing sensitivity, not a deterministic regression.

Repro:

```
cargo build
for i in $(seq 1 24); do
  (timeout 30 target/debug/mutsu t/supply-done-in-tap-callback-is-not-a-failure.t \
     >/dev/null 2>&1; echo "exit=$?") &
done; wait
# ~10/24 exit=1 on a loaded machine
```

Next step: root-cause the timing hole (test 3 asserts a `done` emitted from
inside a tap callback is not treated as a failure — likely a race between
the tap callback thread and the supply completion bookkeeping), or if it
proves genuinely non-deterministic by design, quarantine it via
`flaky-tests.txt` following docs/flaky-test-policy.md (this measurement
satisfies the evidence standard; note the 10/24 rate).
