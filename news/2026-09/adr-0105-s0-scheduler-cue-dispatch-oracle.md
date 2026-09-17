# ADR-0105 S0: pinned the promise-scheduler cue-dispatch oracle

Landed the first implementation slice of [ADR-0105](../../docs/adr/0105-promise-resolution-dispatches-through-the-promise-scheduler.md)
(the design that answers [#8380](https://github.com/tokuhirom/mutsu/issues/8380),
the `Test::Time` / `Test::Scheduler` virtual-time deadlock): S0, "pin the
oracle."

`t/concurrency/thread-lock/scheduler-cue-dispatch-oracle.t` adds a logging
`Scheduler` class and pins the ADR's Appendix A measurements against
mutsu's current behavior, each as a `todo`-marked assertion naming the
slice expected to make it pass:

- **F4a** — `Promise.scheduler` should expose the scheduler a promise was
  constructed with. It doesn't exist yet.
- **F1a** — keeping an awaited promise should cue the awaiter's wake-up
  through the promise's bound scheduler. mutsu's `await` is a blocking
  condvar wait that never consults a scheduler.
- **F1b** — a `.then` callback should be dispatched through the promise's
  scheduler too. Same gap.
- **F4b** — a `start` block under a user `$*SCHEDULER` should cue its task
  through it. mutsu always dispatches straight to the built-in worker pool.
- **Experiment 3** — the keeper's own continuation should be observed by
  the woken awaiter before dispatch can overtake it. Rakudo is 30/30; the
  ADR measured mutsu at 28/30, so this assertion may occasionally pass
  today by chance, which is harmless under `todo`.

No `src/` change — this slice is test-only, per the ADR's own S0
definition. As S1 (D1: the scheduler field and its binding sites), S2 (D2:
resolution dispatch through the scheduler), and S4 (D4: deferred start for
worker-submitted tasks) land, their matching `todo` markers come off this
file, turning it into the slices' acceptance check.
