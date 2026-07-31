# Write a crash report when the interpreter dies of a fatal signal

When mutsu segfaults, CI tells us nothing beyond `Wstat: 11 (Signal: SEGV)`. That is what happened
to `roast/S17-procasync/stress.t` (see [procasync-stress-segv.md](../deep/procasync-stress-segv.md)):
a rare crash on CI, not reproducible locally in 22 attempts, and the wait status alone left two basic
questions unanswered — *which* process faulted (the `.t` file's interpreter, or the subprocess
`is_run` had spawned?) and *where*. A crash that rare has to yield its evidence the one time it
fires; there is no second chance to attach a debugger.

## What to build

Install a handler for the fatal signals (SIGSEGV, SIGBUS, SIGILL, SIGFPE, SIGABRT) at startup and
have it write a per-process crash report, then re-raise so the wait status is unchanged.

- **Use `sigaltstack` + `sigaction` with `SA_ONSTACK`.** The alternate stack is not optional: a
  stack-overflow SIGSEGV leaves no stack for the handler to run on, and without it the handler simply
  never runs.
- **Write to a file, not stderr** — `tmp/crash/<pid>.txt` or similar. Under `prove` the output is
  merged and captured, and in the motivating case the TAP stream was already derailed; a file
  survives regardless.
- **Record, at minimum:**
  - signal number and `si_addr` (the fault address) — distinguishes a null deref from a freed/garbage
    pointer at a glance
  - **pid and argv** — this alone answers the parent-vs-`is_run`-child question that the procasync
    note had to leave open
  - the faulting thread's id/name
  - a backtrace (best effort, see below)
- **Re-raise the original signal** after writing, restoring the default disposition, so the exit
  status and any core dump behaviour stay exactly as they are today.

`libc` is already a dependency of the default `native` feature, so no new crate is strictly required
(the `backtrace` crate would be if we want `trace_unsynchronized`).

## Cost

Two syscalls at startup and nothing else until a crash. Gate it behind `MUTSU_CRASH_REPORT=1` (set in
CI) if even that is unwanted in the shipped binary — but the steady-state cost is genuinely nil, so
default-on is defensible.

## Known limitations — do not oversell this

- **The handler is not async-signal-safe.** `std::backtrace::Backtrace::force_capture()` allocates
  and takes locks, so if the fault happened inside the allocator the handler can deadlock or fault
  again. Mitigate with `alarm(10)` before capturing (a wedged handler still dies) and/or the
  `backtrace` crate's `trace_unsynchronized`. Accept that the backtrace is best-effort; the signal
  number, fault address, pid and argv are the parts that will reliably land, and they are already
  more than we have today.
- **`[profile.release]` sets `debug = false`** (deliberately — full debuginfo costs ~250MB per binary
  and ~70s of link time, see the comment in `Cargo.toml`). So symbolication in the build that
  `make roast` actually runs will be poor. Do **not** flip `debug = 1` as part of this ticket: ship
  the handler first, look at what a real report contains, and only then weigh line tables against
  binary size, build time, and the risk that changing the build perturbs the timing of a
  concurrency Heisenbug.
- **This is instrumentation, not a fix.** At roughly one occurrence in several dozen CI runs, nothing
  happens until the bug next appears. The value is that the next occurrence produces an answer
  instead of another dead end.

## CI wiring

Small: `ci.yml` already has the pattern. Add an `if: failure()` step that cats any
`tmp/crash/*.txt` into a log group and an `actions/upload-artifact` step alongside the existing
`roast-log` upload.

## Follow-ons (separate tickets, not this one)

- **Core dumps in CI.** GHA runners have sudo, so `ulimit -c unlimited` plus a `kernel.core_pattern`
  override (Ubuntu's apport must be displaced) would let a failure step run
  `gdb -batch -ex 'thread apply all bt'` and upload the text. The advantage over the in-process
  handler is *all* threads' stacks, which matters for a concurrency bug. Same debuginfo caveat.
- **A sanitizer job.** A scheduled (not per-PR) workflow running the S17 subset in a loop under
  `-Zsanitizer=address` would name the exact bad access, which is what actually root-causes a
  GC/concurrency bug. Materially more work: nightly toolchain, a 2-3x slowdown that may stop the race
  reproducing at all, triage of the GC's deliberate raw-pointer writes, and suppressions for
  libffi/native calls. Worth doing only once there is a reason to believe it will fire.
