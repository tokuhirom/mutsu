# A late `await` waits for its keeper's yield too

ADR-0105 D2 deferred the wake-up of an `await` parked on a promise until the
pool worker that kept it yielded, so the woken thread could not overtake the
keeper's own straight-line code. An `await` that only *reached* the promise
after the keep got no such ordering: it saw the promise resolved, returned at
once, and raced the keeper. That was the real cause of the experiment-3
subtest of `t/concurrency/thread-lock/scheduler-cue-dispatch-oracle.t`
failing 15 times in 200 under debug-build/JIT contention (#10016) — the
wall-clock tick the issue suspected never fired in the instrumented failures.

A resolution on a pool worker now records a `KeeperMark` (the worker's
deferred list and its yield epoch, which every yield bumps under the list's
lock). An `await` that finds the mark still current defers its own release
onto the keeper's list and parks; one that finds the epoch moved on, or that
runs on the keeper itself, returns at once. ADR-0105 §9 states the resulting
invariant — an `await` returns no earlier than the resolving worker's next
yield, with only the `WAKE_GRACE` liveness fallback excepted — and why
ordering under arbitrary keeper preemption is deliberately not promised.
Deterministic regression tests: `yield_points::tests` and
`t/concurrency/promise/await-after-keep-keeper-order.t`.
