# An unhandled exception in a sunk `start` ends the program

```raku
start { die "dead" }; sleep 1; say "still alive"
```

Rakudo reports `Unhandled exception in code scheduled on thread N`, the
message and the backtrace, then exits with status 1, so `still alive` is
never printed. mutsu printed the diagnostic but kept running and exited 0
(#9767).

A sunk `start` promise is now fatal at the moment both of these are true:
the block has died, and the promise has been sunk. Whichever happens last
decides it, under the promise's own lock, so the report fires exactly once.
If the worker breaks a promise that was already sunk, the worker exits.
If the mainline sinks a promise that has already broken, the mainline exits
(`OpCode::MarkPromiseSink`). The worker's buffered output is written first.
A promise kept in a variable, awaited, inspected or chained with `.then`
stays observable and is not fatal. A user
`$*SCHEDULER.uncaught_handler` replaces the default exit. The old report
from the promise's destructor is gone. It also printed when a user
`uncaught_handler` had already handled the exception.

Exiting from the worker does not run END phasers yet; that is #11617.
