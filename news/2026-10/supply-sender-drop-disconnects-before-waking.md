# A dropped supply sender disconnects before it wakes the taps

`t/concurrency/thread-lock/service-thread-refusal.t` hung now and then, in CI as
well as locally. The hang came in the block where `Proc::Async.start` has its
stdout reader thread refused. The `proc-wait` helper killed the child and then
waited for the stdout tap's act loop to finish, and that loop never woke again.

`SupplySender`'s `Drop` woke the channel's drain hooks so an event-driven act loop
could notice the hangup. Rust drops a struct's fields only after `drop()` returns,
though, so the wake came while the inner `mpsc::Sender` was still alive. A drain
scheduled by that wake could read the still-connected channel as `Empty` and go
idle. The real disconnect then woke nobody. The sender is now dropped before the
wake.

Under a 4-wide stress loop the refused-reader snippet hung 4 times in about 44 runs
before the fix and 0 times in 200 runs after it. A unit test pins that the wake
sees the channel already disconnected (#13015).
