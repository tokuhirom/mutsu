# Crash reports from subprocesses now reach CI's collection step

The [fatal-signal crash report](crash-report-on-fatal-signal.md) works for every mutsu in a job — any
`t/` or roast file's own interpreter, and any subprocess it spawned, each writing its own
`<pid>.txt` told apart by pid and argv. Verified end to end: a parent that spawns a crashing child
survives with exit 0 while the child leaves a report naming *its* argv, which is precisely the
parent-vs-`is_run`-child question the procasync note could not answer.

One case escaped collection, though. The default report directory `tmp/crash` is *relative*, and it
is resolved against each process's **startup** working directory. A later `chdir` therefore cannot
move it — that part was already right — but a process that *starts* somewhere else does not report
where CI looks:

```
$ mutsu -e 'run($*EXECUTABLE, ..., :cwd("tmp/elsewhere"))'   # child faults
tmp/crash/                       -> does not exist
tmp/elsewhere/tmp/crash/1704623.txt   <- the report, where nothing collects it
```

The same happens to a child that inherits a parent's `chdir`. Both shapes occur in the suite, and a
subprocess crash is exactly what the feature exists to attribute — so losing it would have hollowed
out the interesting half of the coverage.

The fix is one line per job: `MUTSU_CRASH_DIR` is now exported as an **absolute** path
(`${{ github.workspace }}/tmp/crash`) in the `test`, `gc-stress` and `jit-stress` jobs. Environment
is inherited down the process tree, so every descendant — however it was spawned and wherever it
starts — writes into the single directory the crash-report step prints and uploads.

`tests/crash_report.rs` pins the property: a mutsu started in a different working directory with an
absolute `MUTSU_CRASH_DIR` reports into that directory, records its own `cwd` in the report, and
writes nothing under the directory it ran in.
