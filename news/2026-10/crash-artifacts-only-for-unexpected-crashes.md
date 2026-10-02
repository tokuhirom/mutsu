# Crash-report artifacts now appear only for unexpected crashes

Every roast job on a green CI run used to upload a `crash-reports` artifact
(`crash-reports`, `gc-stress-roast-crash-reports`,
`jit-stress-roast-crash-reports`). Each held one report: a SIGSEGV inside
libc's `strdup`. That crash is deliberate. `roast/S29-os/system.t`
(rakudo#3149) spawns `mutsu -e '... strdup(0)'` on purpose and asserts that
the parent's `run()` reports the segfault as `X::Proc::Unsuccessful`.
`scripts/report-crash-reports.sh` already allowlisted that argv and did not
fail the job. Still, the artifact on a green run, plus an `::error::` line
counting it, made it look as if a real crash had been left unattended.

The script now sorts the reports first. Allowlisted reports move to
`tmp/crash/expected/`, are printed folded in the log, and produce only a
`::notice::`. `::error::` annotations and the step summary cover unexpected
reports only. All five upload steps in `ci.yml` now take `tmp/crash/*.txt`,
so a crash-report artifact exists only when something crashed that no test
asked for. Such a crash still fails the step, as before.
