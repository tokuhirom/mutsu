# `Thread.start` and `Proc::Async` readers capture celled aggregates by cell

ADR-12204 slice S2, step 2 continued. The thread that runs a `Thread.start`
body and the consumer thread of a `Proc::Async` stdout/stderr tap are created
with `clone_for_thread_for_block`, so the body/callback is their capture
source: the `@`/`%` lexicals it references that live in a shared cell are
captured by the cell and get no name-keyed lane entry. Supply drivers and the
hyper/race batch threads (no single callback) are what is left of step 2.
