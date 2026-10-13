# A worker's `exit` interrupts a `run` waiting for its child

When a worker thread calls `exit`, the main program unwinds, runs `END`, and exits with the
worker's status (ADR-11617). A main thread blocked in `run 'sleep', '3'` did not notice that
for its one-second grace period, so the worker ended the process without `END`. Child-process
waits (`run`/`shell` without capture, `Proc.exitcode`, closing a piped handle) now give up on
a pending worker `exit` too. The child is left running, as after any Raku `exit` (#13092).
Blocking reads, such as `$*IN.get` or slurping a `run :out` pipe, are still in the gap (#13100).
