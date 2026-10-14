# A Condition signal is no longer lost when it lands as the poll times out

The main program's thread waits on a `Lock::ConditionVariable` in 10 ms slices so it can adopt a
worker's `exit`. std's `Condvar` reports a notification that arrives while the waiter is reacquiring
the mutex after a timeout as `timed_out()`, and the wait loop then slept again for a signal that was
already gone: `t/concurrency/thread-lock/loop-scoped-lock-condition.t` hung about once in 256 runs
under load (#13184). Each condition now keeps a signal count in the lock state, bumped by
`signal`/`signal_all` under the state mutex; a waiter that timed out compares it with the count it
saw before parking, so a signal delivered in that window still wakes it.
