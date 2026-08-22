# `t/promise-supply-coercion-async-drive.t` test 3 fails under CPU oversubscription

Measured 2026-08-22 while triaging a `jit-stress` red on PR #6841 (whose diff was
proven unrelated — see below). The failure is real non-determinism in this test,
not in the PR that happened to surface it, and it will keep reddening CI at
random until someone root-causes it.

## Symptom

`jit-stress` (`MUTSU_JIT=on`, `MUTSU_JIT_THRESHOLD=2`, debug build) reported:

```
t/promise-supply-coercion-async-drive.t   (Wstat: 256 (exited 1) Tests: 3 Failed: 1)
  Failed test:  3
```

Test 3 is the "same-thread producer + coercion-awaiter does not deadlock" case:
a `start` block creates `Promise(supply { whenever $s.Supply ... })`, feeds and
`done`s the source, `await`s the coerced promise into `$result`, then keeps
`$done`; the main thread does `await Promise.anyof($done, Promise.in(5))` and
asserts `$done.status eq 'Kept' && $result eq 'xy'`.

## Reproduction

Deterministically reproducible *as a rate*, not as a single run. With the
jit-stress env and 16 busy-loop processes on the box (16x CPU
oversubscription), running the file 20 times:

- **8 / 20 failures**, always test 3 only, tests 1 and 2 always pass.
- Without the load: **0 / 10** failures (and 0/10 with `prove` too).

## It is NOT simply the 5-second budget

The obvious hypothesis — "the `Promise.in(5)` deadline loses the race on a
saturated runner" — was tested and **rejected**: raising it to
`Promise.in(30)` did not reduce the failure rate (13/20 under the same load,
and the failing runs still finished quickly rather than sitting out the
deadline). So the `start` block is not merely slow; something is not becoming
visible to the main thread at all.

Two further observations point the same way:

- one failing run also printed `Use of Nil in string context` at line 23 — i.e.
  `await($p)` in the *first* block returned `Nil` under load, which is the same
  "cross-thread result not observed" shape a level down;
- the assertion reads two lexicals (`$result`, `$done`) written on the `start`
  thread, which is exactly the dual-store / cross-thread lexical writeback area
  that has produced several past bugs (see `t/lock.t` in CLAUDE.md's de-flaked
  list, fixed in #4167).

The likely root cause is therefore a lost or unsynchronised cross-thread write
of a `start`-block lexical (or a `Promise.anyof` that reports the anyof as
resolved before the underlying `keep` is visible), not a timing budget. That
needs a real investigation, which is why this is a ticket rather than a
one-line timeout bump — bumping the timeout was measured and does not fix it.

## Why the PR that surfaced it was not the cause

PR #6841 added a `__SupplyQuitForwarder` reached only from the `"tap" | "act"`
dispatch's b3 branch (a `whenever` whose source is an on-demand `supply` block).
This test's `whenever $s.Supply` is supplier-backed (b1). A `rust-gdb -batch`
breakpoint on `Interpreter::build_supply_quit_forwarder` never fired across a
full run of this file, so the new code is provably not executed here; the rest
of that diff is additive class registration. The same job passed on retry.

## Suggested next steps

1. Reproduce with the load harness above (16 spinners, jit-stress env, 20 runs).
2. Instrument the `start`-block lexical writeback for `$result` / `$done`
   (per CLAUDE.md, prefer a `rust-gdb` breakpoint on the writeback site over an
   `eprintln!`) and confirm whether the write happens and is simply not visible,
   or never happens.
3. If `Promise.anyof` is resolving early, pin that separately — it would affect
   far more than this test.
4. Until root-caused, consider a `flaky-tests.txt` quarantine entry with a review
   date (the evidence standard in `docs/flaky-test-policy.md` is met: measured
   8/20 under load, 0/10 unloaded, passes on CI retry).
