# `scripts/dev`: long jobs and the pre-publication gate get a real runner

[ADR-0126](../../docs/adr/0126-dev-job-runner-for-long-jobs-and-gates.md).

Agent sessions spend much of their wall clock on `make test`, `make roast`, `make lint` and release
builds. Until now each session improvised how to start them, wait for them and read the result. The
same few improvisations kept failing:

- `pgrep -f "make lint"` matched the shell running it, so a wait never ended.
- `pkill -f "make test"` killed its own caller.
- A leftover `exit=` line was nearly read as the result of the next run.
- A container restart killed a detached run and nothing noticed.
- The roast verdict was compared by eye against a prose table.

`scripts/dev` replaces all of that:

```sh
scripts/dev gate                 # fmt --check, make lint, make test, make roast: one job
scripts/dev wait <id>            # returns within 9 minutes; 75 = still running, wait again
scripts/dev status | log | stop | report
scripts/dev run <name> -- <cmd>  # any other long job
```

- A job is a directory under `tmp/jobs/`. The runner tracks it by its recorded pid and start time,
  never by process name, and reports a job that died without finishing as `lost`.
- Only one job of each name can run at a time.
- A gate result is keyed by `git write-tree`, so running `gate` again on an unchanged tree reports
  the stored result instead of re-running.
- The gate writes a `report.json` verdict. The remote container's four environment-only roast
  failures are now data (`ci/known-env-failures.toml`), matched by exact shape, so a known file
  failing in a new way is still reported.
- `AGENTS.md` and the skills now point at it, and `make check-dev` (also in CI) self-tests the
  runner.

The first gate run found a real divergence straight away. CI runs `cargo test` on an 8 MiB stack,
but `make test` used Rust's 2 MiB default, so a debug test overflowed locally only. `make test` now
matches CI.
