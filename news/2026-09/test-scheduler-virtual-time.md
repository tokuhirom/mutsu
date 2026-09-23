# Test::Scheduler virtual-time tests gain cancellation and scheduler parity

The ecosystem roulette locked `Test::Scheduler` 1.2 on #8977. Its official
suite sweep moved from 0/3 to 2/3 baseline files at parity and from 88/95 to
94/95 assertions under mutsu.

Mutsu now gives user-created `Cancellation` objects a real cancelled state and
routes `Supply.interval` through a user-defined dynamic `$*SCHEDULER` when no
explicit scheduler is supplied. The new behavior is pinned by
`t/concurrency/thread-lock/scheduler-cancellation-state.t` and
`t/concurrency/supply/supply-interval-dynamic-scheduler.t`.

The remaining `synopsis.rakutest` failure is the scheduler continuation-ordering
work tracked by #8380. Repeated direct runs also show the
`not-time-based.rakutest` assertion is flaky under the same scheduler race.
