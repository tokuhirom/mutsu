# A fatal signal on a worker thread finally writes a crash report

mutsu writes a crash report for a fatal signal (`src/crash_report/`) so that the one time a rare
SIGSEGV fires on CI it leaves a thread name, an argv line and a backtrace behind instead of a bare
`Wstat: 11`. Three times — CI runs 32968762746, 33154831894 and the original 30590633128 — the
mechanism failed to capture the very crash it was installed for, and each recurrence cost a red CI
run and yielded nothing (`todo/deep/procasync-stress-segv.md` §8.2).

## It reproduces in one line

No race needed:

```
$ MUTSU_CRASH_DIR=$PWD/tmp/crash mutsu -e 'use NativeCall; sub strdup(int64) is native(Str) {*}; strdup(0)'
tmp/crash/1034030.txt        # main thread: report written
$ MUTSU_CRASH_DIR=$PWD/tmp/crash mutsu -e 'use NativeCall; sub strdup(int64) is native(Str) {*}; await start { strdup(0) }'
(nothing)                    # worker thread: no report
```

## The cause: the handler overflowed its own alternate signal stack

`strace -f -e trace=sigaltstack` settles it in four lines (the worker is pid 1035196):

```
1035196 sigaltstack({ss_sp=0x72daeb614000, ss_flags=0, ss_size=8192}, NULL) = 0
1035196 --- SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_MAPERR, si_addr=NULL} ---
1035196 --- SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_ACCERR, si_addr=0x72daeb613848} ---
1035196 +++ killed by SIGSEGV (core dumped) +++
```

`ss_size=8192` is **`std`'s** alternate signal stack: Rust installs a `SIGSTKSZ`-sized one on every
thread it spawns, sized for its own guard-page check. mutsu's 256 KiB stack was installed only by
the threads that call `crash_report::install()` — the process's first thread and `mutsu-main` — so a
worker kept `std`'s. mutsu's handler needs more than 8 KiB (a 4 KiB path buffer, a 2 KiB header
buffer, and the raw-backtrace frames), so on a worker it overflowed the alternate stack: the second
SIGSEGV is at `0x…613848`, 0x7b8 bytes *below* the alternate stack's base. A fault inside a signal
handler is fatal and silent, so the process died with nothing written.

The signal *disposition* was never the problem. It is process-wide, and a `rust-gdb` breakpoint on
the handler confirmed it did run on the worker — it just could not survive long enough to open the
file. Of §8.2's three candidates this is the second one; the first ("no handler on that thread") and
third ("`MUTSU_CRASH_DIR` did not reach the process") are both eliminated.

## The fix

Every thread mutsu spawns goes through one funnel, `builtins_system::spawn_registered_thread`, so
that is where each worker now takes `crash_report::install_thread_alt_stack()` — an RAII guard that
installs the 256 KiB stack and puts the previous one back when the thread exits, so a short-lived
worker leaks nothing. It installs no handler: that half is process-wide and already done.

| crash | before | after |
| --- | --- | --- |
| main thread | `thread: mutsu-main` | unchanged |
| `start` block | *no report* | `thread: pool` |
| `Thread.start` | *no report* | `thread: raku-thread` |
| worker stack overflow | *no report*, bare SIGSEGV | `thread: pool` — **and** `std`'s "thread 'pool' has overflowed its stack" message, which the too-small stack was swallowing too |

That last row is a second bug fixed by the same change: a worker's stack overflow used to die as an
uninformative SIGSEGV, where the main thread's aborts with a diagnosis.

## Pins

`tests/crash_report.rs` grew `a_segv_on_a_worker_thread_is_reported` and
`an_abort_on_a_worker_thread_is_reported`, driven by two new selftest modes
(`MUTSU_CRASH_SELFTEST=segv-thread` / `abort-thread`) that fault on a thread spawned through the
production `spawn_user_thread` path — so the test proves the *real* spawn installs the stack, not
merely that one can be installed. Both assert the report's `thread:` field names the worker and its
`tid:` differs from `pid:`.

## What this does not fix

`todo/deep/procasync-stress-segv.md` stays open: the underlying `advent2014-day05.t` / `stress.t`
crash is still un-root-caused. What changed is that its next recurrence arrives with a thread name
and a backtrace.
