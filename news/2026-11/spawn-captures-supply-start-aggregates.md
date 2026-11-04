# `Supply.start` and `throttle` workers capture celled aggregates by cell

ADR-12204 slice S2, step 2 continued. The worker that runs `Supply.start`'s
per-value call, the `throttle` coordinator and a mutated supply tap driver are
created with `clone_for_thread_for_block`, so their callable is the capture
source: the `@`/`%` lexicals it references that live in a shared cell are
captured by the cell and get no name-keyed lane entry. Drivers that run no user
callable (`stable`, `delay` windows) and the hyper/race batch threads stay on the lane.
