# A fatal signal now writes a crash report

When mutsu died of a signal, CI told us nothing beyond `Wstat: 11 (Signal: SEGV)`. That is what
happened to `roast/S17-procasync/stress.t` (see
[todo/deep/procasync-stress-segv.md](../../todo/deep/procasync-stress-segv.md)): a crash seen once in
several dozen CI runs, not reproducible locally in 22 attempts, and the wait status alone left two
basic questions unanswered — *which* process faulted (the `.t` file's interpreter, or the subprocess
`is_run` had spawned?) and *where*. A crash that rare has to yield its evidence the one time it
fires; there is no second chance to attach a debugger.

`src/crash_report/` now installs a handler for SIGSEGV, SIGBUS, SIGILL, SIGFPE and SIGABRT that
writes a per-process report to `tmp/crash/<pid>.txt` before letting the signal through:

```
mutsu crash report
signal: 11 (SIGSEGV)
si_code: 1
fault-addr: 0x0000000000000000
pid: 1554922
ppid: 1554921
tid: 1554923
thread: mutsu
time: 1785484046
version: 0.20.0
cwd: /home/tokuhirom/work/mutsu-codex
argv: target/release/mutsu roast/S17-procasync/stress.t
--- backtrace (raw) ---
...
--- backtrace (symbolized, best effort) ---
...
```

`argv` alone answers the parent-vs-child question the procasync note had to leave open, and the fault
address separates a null deref from a freed or garbage pointer at a glance.

## The parts that make it actually fire

- **`sigaltstack` + `SA_ONSTACK`.** Not optional: a stack-overflow SIGSEGV leaves no stack for the
  handler to run on, so without an alternate stack the handler simply never runs. Ours is 256 KiB,
  well past the platform `SIGSTKSZ`, because the symbolized backtrace is stack-hungry.
- **A file, not stderr.** Under `prove` the output is merged and captured, and in the motivating case
  the TAP stream was already derailed. A file survives regardless.
- **Async-signal-safe down to the raw backtrace.** The header, the `mkdir -p` of the report
  directory, the `open`, and glibc's `backtrace_symbols_fd` use nothing but syscalls and fixed stack
  buffers — no allocation, no locks, no `core::fmt` — so the report lands even when the fault was
  inside the allocator. Only the symbolized backtrace breaks that rule, and it is written last,
  behind an `alarm(10)`, after everything that matters is already on disk.

## Hand-off, not re-raise

The handler must leave the exit status, `std`'s stack-overflow message and core-dump behaviour
exactly as they were. The obvious `raise(sig)` does not: it hands the restored handler an `SI_TKILL`
siginfo with a null address, which makes `std`'s guard-page check miss a stack overflow and swallow
its `has overflowed its stack` message — turning a SIGABRT into a SIGSEGV. So the handler restores
the previous disposition and then, for a genuine hardware fault (`si_code > 0`), simply **returns**:
the faulting instruction re-executes and faults again, and the restored handler sees the true
`si_addr`. Only a signal that was *sent* (`kill`, `raise`, `abort`) is re-raised, because that one
has no instruction to re-execute. Measured: the wait status is identical with and without the
handler for a null deref (139), an `abort()` (134) and a real stack overflow (134, message intact).

## Cost

A handful of syscalls and one allocation at startup, and nothing at all until a crash. The report
directory is created *from inside the handler*, so an ordinary run in an arbitrary working directory
leaves no trace — pinned by a test. `MUTSU_CRASH_REPORT=0` disables the feature entirely and
`MUTSU_CRASH_DIR` relocates the reports.

## CI and tests

`scripts/report-crash-reports.sh` prints every report into the job log and a summary table; the
`test`, `gc-stress` and `jit-stress` jobs run it on failure and upload `tmp/crash/` as an artifact.
`tests/crash_report.rs` drives the whole pipeline for real through a `MUTSU_CRASH_SELFTEST` hook —
asserting the report contents, the file name, the unchanged wait status, the opt-out, and that a
clean run creates nothing.

## What this is not

Instrumentation, not a fix. At roughly one occurrence in several dozen CI runs nothing happens until
the bug next appears; the value is that the next occurrence produces an answer instead of another
dead end. Two heavier follow-ons stay unbuilt on purpose, and should only be built if a real report
says they are needed: **core dumps in CI** (`ulimit -c unlimited` plus a `kernel.core_pattern`
override, giving *all* threads' stacks) and **a scheduled sanitizer job** (which would name the exact
bad access, at the cost of a nightly toolchain, a 2-3x slowdown that may stop the race reproducing at
all, and suppressions for the GC's deliberate raw-pointer writes and for libffi).

Symbolication in the build `make roast` runs is poor, because `[profile.release]` sets
`debug = false` deliberately (full debuginfo costs ~250MB per binary and ~70s of link time). That was
left alone: ship the handler, look at what a real report contains, and only then weigh line tables
against binary size, build time, and the risk that changing the build perturbs the timing of a
concurrency Heisenbug. The raw frame addresses are resolvable offline in the meantime with
`addr2line -f -e target/release/mutsu <address>`.
