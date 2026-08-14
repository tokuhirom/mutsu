# A failing test file prints `Runtime error: Test failures` on stderr

Split out from `todo/tickets/retire-native-test-util-overrides.md` (now
resolved and moved to `news/2026-08/retire-native-test-util-overrides.md`) as
an unrelated leftover noticed during that work, not fixed there.

When a `t/*.t` (or roast) file ends with one or more failing assertions,
mutsu prints an extra `Runtime error: Test failures` line to stderr that
rakudo does not produce. `run()` returns the failure as a `RuntimeError` and
`main` renders it as an error, rather than treating "some assertions failed"
as an ordinary non-zero exit.

The exit status is already correct (1), so the fix is to set `exit_code` and
return `Ok` from wherever this path currently returns `Err`, the same way the
bailed-out (`bail-out`) branch already does.

Nothing currently asserts on the absence of this line, so it has not caused a
test failure yet — but any future `is_run`/`get_out`-based test that checks a
failing subprocess's stderr for exact equality (rather than a substring
match) will hit it.
