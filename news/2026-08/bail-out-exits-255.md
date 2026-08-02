# `bail-out` exits 255

Rakudo's `Test.rakumod` ends `bail-out` with `exit 255` right after emitting the
`Bail out!` line. mutsu's native provider emitted the line, set the interpreter's
`halted` flag and marked the TAP state as bailed out — and then returned a clean
`Ok(())` from the run loop, so the process exited 0. `prove` therefore read a
bailing-out file as a successful run, and `Test::Util`'s
`is_run ..., :255status` — which is what `roast/S24-testing/7-bail_out.t`
asserts on all four of its subtests — saw status 0.

The bailed-out early return now sets `exit_code = 255` before returning, which
is the same channel the "planned N but ran M" dubious case already used.

Pin: `t/bail-out-exit-status.t` (both the bare and the `bail-out "reason"`
forms, checking status and the emitted output), byte-compatible with `raku`.
