# `Supply.throttle(:control)` listens before its worker starts

`Supply.throttle(0, &block, :$control)` holds every job until a `limit:N`
directive arrives on `$control`. That directive is usually emitted on the line
right after `throttle`, and a live `Supplier` with no taps keeps no history to
replay. But the throttle registered its listener from inside the worker task it
submitted. When the pool was busy that task started after the emit, missed it,
and held the jobs for its whole 30-second bound. This is why
`roast/S17-supply/throttle.t` failed tests 9-10 in CI's parallel roast run
(#18181) and passed whenever the file ran alone.

The listener (`ControlLimitSink`) is now registered by the thread that builds
the throttle, before the worker is submitted, and the worker only waits on it.
A new test busies the pool first, so the old race happens every time: before
the fix it failed 5/5 runs, now it passes.
