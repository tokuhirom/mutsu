# Promise wake-ups go through the promise's scheduler; Test::Time no longer deadlocks

ADR-0105 is implemented. `Test::Time` 0.0.2's `t/01-tdd.t` used to deadlock
under the default worker pool (#8380). It now passes 6/6. All three
`Test::Scheduler` 1.2 test files pass too.

Rakudo sends every promise subscriber through `$promise.scheduler.cue`. That
covers `.then` callbacks and the wake-up of a parked `await`. mutsu now does
the same:

- **User schedulers see the wake-up.** Keeping a promise bound to a user
  scheduler cues one dispatcher block through that scheduler's `.cue`. The
  dispatcher runs the callbacks and wakes the awaiters in the order they
  were registered.
- **The woken awaiter runs to its next blocking point first.** The cued
  dispatcher lends the awaiter its turn. It does not finish until the
  awaiter reaches its next blocking point or the end of its task. A
  virtual-time scheduler like `Test::Scheduler` depends on this: its
  `advance-by` must not return until the woken code has registered its next
  `sleep`.
- **Built-in wake-ups wait for the keeper to yield.** When a pool worker
  keeps a promise, the wake-up is delivered at the worker's next blocking
  point or task end. So `start { $p.keep; $flag = 1; sleep 1 }` is always
  seen with `$flag == 1` by the thread that awaited `$p`.
- **Worker-submitted tasks wait their turn.** When no worker is idle, a
  task submitted from a running pool worker starts when that worker yields,
  or on a 10ms tick if it never does.

The hot `await` path uses the same number of threads and hops as before.

Getting `Test::Scheduler` green also turned up two older bugs where threads
saw stale variable values. Both are fixed:

- A `start` inside a method used to copy the method's attribute aliases
  into the cross-thread variable store. After that, a
  `$!lock.protect: { @!x ... }` block read the copy taken when the thread
  started.
- A protect block could read a re-declared `@`/`%` variable's entry from an
  earlier call of the same routine.
