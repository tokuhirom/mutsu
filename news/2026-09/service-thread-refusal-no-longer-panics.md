# A refused service thread is a catchable X::AdHoc, not a panic

ADR-0123 made an OS-refused *user-code* thread a catchable `X::AdHoc`, but left
the default-stack service threads on a panicking `spawn_gc_helper_thread`
wrapper (its "Remaining panics" note, #9401). Under `RLIMIT_AS` /
`RLIMIT_NPROC` pressure the same `EAGAIN` still killed the whole process from
`Promise.in`, `Supply.interval`, a delayed `$*SCHEDULER.cue`, `signal(...)`,
`IO::Path.watch`, `IO::Socket::Async.listen` and connection reads, and
`Proc::Async.start`.

Every call site now uses `try_spawn_gc_helper_thread`, and the wrapper is gone:

- A call that returns a value raises the refused-thread `X::AdHoc`
  (`Could not create a new Thread: ...`), dropping the supply channel it had
  registered. `listen` reports it through the tap's `quit` when there is one,
  the way a failed bind already did.
- The two process-lifetime threads -- the timer driver and the signal reader
  -- used to be spawned inside a `OnceLock` initializer, which cannot fail.
  They now start through an `ensure_*` step that leaves them unstarted on
  refusal, so the next registration tries again.
- `Proc::Async.start` already has a running child when a helper is refused.
  A refused stdin feeder or `proc-wait` kills and reaps the child (by pid for
  `proc-wait`, whose `Child` handle went down with the refused closure) and
  returns a broken promise; a stdout/stderr reader refused from inside
  `proc-wait` kills the child and breaks the promise instead of keeping it.

Because an OS limit cannot refuse these threads deterministically, the new
`MUTSU_REFUSE_SERVICE_THREADS` (a comma-separated list of thread names, or
`all`) injects the refusal. Pin:
`t/concurrency/thread-lock/service-thread-refusal.t`.
