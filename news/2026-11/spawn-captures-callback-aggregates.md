# Callback spawns capture celled aggregates by cell too

ADR-12204 slice S2, step 2. The threads that run a scheduler cue callback, a
channel-backed `tap` callback, a socket listener's accept callback and a nested
`whenever <Promise>` body are now created with `clone_for_thread_for_block`
instead of the block-less `clone_for_thread`, so the callback is their capture
source: the `@`/`%` lexicals it references that live in a shared cell are
captured by the cell and get no name-keyed lane entry. Spawns with no single
callback (supply drivers, `Proc::Async` readers, `.then` chains) stay on the lane.
