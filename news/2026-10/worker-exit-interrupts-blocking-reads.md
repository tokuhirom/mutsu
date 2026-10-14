# A worker's `exit` interrupts a blocking read on the main thread

When a `start` block calls `exit` while the main program is blocked reading standard input
(`$*IN.get`) or the output of a `run ... :out` child that is still running, the main program now
stops the read, runs its END phasers and exits with the worker's status, as Rakudo does. Before,
the read could not be woken: the worker waited out its one-second grace period and ended the
process without END.

On the main thread the read first waits for its file descriptor with `poll(2)`, re-checking for a
pending worker `exit` every 10 ms. Standard input is only waited on while std's buffer is empty,
so a line that is already buffered is returned at once (#13100, ADR-11617).
