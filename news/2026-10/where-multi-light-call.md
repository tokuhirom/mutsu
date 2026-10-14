# A `where`-constrained multi call skips the generic dispatch path

A multi family with a `where` clause could not use the cached light call path
that type-only families take since #10109: its winner depends on the
argument values, so every call ran the whole generic resolution around the
family's cached dispatch plan.

Such a family now keeps its plan and the compiled body of every candidate
its stage program can pick. A repeated call with the same argument types runs
only the value checks and goes straight to the winner's light bind, with the
same `callsame`/`nextsame`/`samewith` behaviour and each `where` still run
exactly once per call. A loop calling `multi f(Int $x where * > 0)` /
`multi f(Int $x)` got 26% cheaper (ADR-12529 §2.4, phase 2 slice 8).
