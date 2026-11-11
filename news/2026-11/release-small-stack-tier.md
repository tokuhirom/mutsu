# Hundreds of parked `start` blocks fit a limited address space

mutsu's `await` holds its thread, so every `start` block parked on a promise
keeps a whole native stack. Under `ulimit -v` the stacks are reserved against
a budget (ADR-0123), and past it a worker that has to exist took the smallest
tier, 32 MiB. Log::Dispatch's `t/040-threaded` parks 500 `start` blocks on one
`await`; under the ecosystem sandbox's 6 GB limit only ~150 threads fit, the
rest were refused, and the test hung for ever.

The 32 MiB floor was sized for a debug build's frames. A release build's are
about a tenth as large (6-17 KiB per Raku call level, measured), so a release
build now steps down through 16, 8 and 4 MiB as well, and ADR-0100's
deep-recursion guard still covers the smallest tier: recursion there raises
the catchable `Too deep recursion` instead of crashing. The 500 parked workers
now finish in 0.2 s under the 6 GB limit, and Log::Dispatch matches rakudo.

This moves the cliff to roughly 700 parked workers rather than removing it;
#17392 stays open for the real fix, a parked task that no longer costs a
native stack.
