# `http-router-named-urls.t` (Cro::HTTP) times out (rc=124) after its own assertions pass

## Context

Not in `roast/` — this is Cro::HTTP::Router's own test suite, checked out
under `tmp/cro-work/`. Referenced as a known, separate issue in the (now
resolved) `closure-for-loop-param-hijacked-by-same-named-captured-outer`
ticket, which fixed a `for`-loop param `GetUpvalue` hijack that made this
file's "Escaped named param" / "Escaped positional" subtests fail. Those
two subtests now pass (verified 2026-08-11) — but the file as a whole still
times out:

```
bash -c 'INC=$(cat tmp/cro-work/inc-paths.txt); \
  timeout 120 target/debug/mutsu $INC -I tmp/cro-work/C_RO_CRO_HTTP_.../t \
    tmp/cro-work/C_RO_CRO_HTTP_.../t/http-router-named-urls.t'
# rc=124, ok=28 (all individually-passing subtests up to that point), no "not ok"
```

## Not yet diagnosed

No repro isolation, no gdb backtrace of where execution is stuck, and no
check of whether this reproduces on a release build (a debug-build timeout
alone is not conclusive — see CLAUDE.md's flaky-triage guidance: confirm
with `target/release/mutsu` before assuming a real hang, since a heavy
debug-build test can simply be slow rather than stuck).

## Suggested attack

1. Confirm with a release build first (rules out "just slow in debug").
2. If still timing out at release speed, `rust-gdb -batch -ex run -ex bt`
   attached to the hung process (or a manual `timeout 30 ... &` + `gdb -p`
   attach) to find the stuck opcode/call.
3. Binary-search the test file's subtests (comment out the back half) to
   narrow which specific assertion or fixture setup after subtest 28 hangs.
