# A failing test file no longer prints `Runtime error: Test failures` on stderr

When a `t/*.t` (or roast) file ended with one or more failing assertions, or
a planned/ran mismatch, mutsu printed an extra `Runtime error: Test
failures` line to stderr that rakudo does not produce:

```
$ mutsu t.t
1..2
ok 1 - a
not ok 2 - b
# Failed test 'b'
# at t.t line 4
Runtime error: Test failures
# You failed 1 test of 2
```

`run()`'s `finish()` returned the failure as a `RuntimeError`, which `main`
rendered as an uncaught error, instead of treating "some assertions failed"
(or a plan mismatch) as an ordinary non-zero exit — the same way the
bail-out branch already did.

## Fix

Both branches now set `exit_code` and `return Ok(())` instead of returning
`Err`, matching the bail-out branch just above them. One subtlety: the
`state.failed > 0` branch must NOT unconditionally set `exit_code = 1` —
`RAKU_TEST_DIE_ON_FAIL` sets a more specific `exit_code = 255` mid-run,
*before* `finish()` ever runs, and the original `Err`-returning code
happened to preserve that (via `main()`'s Err handler only falling back to
1 when `exit_code` was still 0). Preserved explicitly now: only default to
1 when nothing already set a more specific code.

## Tests

`t/runtime-error-test-failures-not-printed.t` (new) — a failing assertion
and a short plan, both asserting exit status and the absence of the
"Runtime error" line. `t/die-on-fail.t` (existing) caught the
`RAKU_TEST_DIE_ON_FAIL` interaction during development — a first version of
this fix regressed its `:255status` expectation to `1` by clobbering the
already-set exit code; full `make test` caught it before landing.
