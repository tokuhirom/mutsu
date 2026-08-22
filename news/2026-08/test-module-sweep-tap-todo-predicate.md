# `test-module-sweep.sh` now recognizes TAP `# TODO` failures as expected

`scripts/test-module-sweep.sh` (the harness that runs every `t/*.t` file
under both mutsu's native `Test` provider and the vendored upstream
`Test.rakumod`, per `todo/deep/vendor-real-test-module.md`) had a
classification bug in its `passes()` predicate: it grepped raw output for
`^not ok` without checking for a TAP `# TODO` suffix, so a legitimate,
expected `not ok N ... # TODO ...` line -- the same thing `prove` and
mutsu's own TAP consumer (`runtime/test_functions.rs`) both tolerate -- was
scored as a genuine failure.

This mattered for files whose *native*-provider baseline legitimately
contains a TODO-annotated `not ok`, such as `t/exits-ok.t` (two negative-case
subtests marked `todo`) and `t/failure-sink-handled.t` (one). Both were
scored as "not passing" even on the native side, so a real difference on the
real-Test side (a truncated plan / `Unknown call: is-approx`) fell into the
sweep's "fail under both" bucket instead of "regressed" -- invisible to the
tool meant to surface exactly that.

The fix: `passes()` now only counts a `not ok` line as a failure when it
lacks a case-insensitive `# TODO` marker:

```sh
grep -E '^not ok' "$out" | grep -qvi '#[[:space:]]*todo' && return 1
```

Verified by hand against both named files: under the fix, `t/exits-ok.t` and
`t/failure-sink-handled.t` now score as passing under the native provider (as
they should) and failing under `MUTSU_REAL_TEST=1` (their real, non-TODO
difference), so they now correctly land in the sweep's "regressed" bucket.

This is a `scripts/`-only test-harness fix; no interpreter code changed. See
`todo/deep/vendor-real-test-module.md`'s 2026-08-23 entry for the full
before/after detail.
