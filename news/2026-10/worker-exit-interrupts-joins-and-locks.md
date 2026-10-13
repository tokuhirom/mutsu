# A worker's `exit` interrupts joins, locks and condition waits on the main thread

Since ADR-11617, a worker thread's `exit` is adopted by the main program, which unwinds,
runs `END` and exits with the worker's status. The main thread noticed the request only
while running bytecode, sleeping, awaiting or receiving from a Channel. Blocked anywhere
else, it waited out the one-second grace period, and the worker ended the process without
`END`.

`Thread.finish`, `Lock.lock`/`.protect`, `Semaphore.acquire` and `Lock::ConditionVariable.wait`
now give up on a pending worker `exit` too. So do the waits that never notice it when the GC
is off. `END { say "end ran" }; my $t = Thread.start({ exit 3 }); $t.finish` now prints
`end ran` and exits 3 right away, as in Rakudo. An interrupted `protect` does not run its
block (#13052).
