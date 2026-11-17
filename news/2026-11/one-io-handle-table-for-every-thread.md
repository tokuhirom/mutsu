# One IO handle table for every thread

ADR-17216 (one program world, shared live by every thread) was accepted, and
its first slice landed: the IO handle table now exists once per process.

Before, every spawned thread (`start`, `Promise.start`, supply drivers, ...)
got a deep copy of the open handles its environment could reach, and a
single-Promise `await` merged the handles the worker had opened back into the
awaiter. That transfer was wrong three ways:

- sibling workers allocated the same handle ids from the copied counter, and
  the merge silently dropped the second worker's handle;
- `await` of an array of promises (and `.result`, and a handle passed back
  through a shared container) never merged, so the handle was "Invalid
  IO::Handle" on the main thread;
- a `close` or a read on an inherited handle only changed the worker's copy.

Now every interpreter holds the same table: ids come from one atomic
counter, the id map has its own lock, and each handle's state has its own
lock, so a blocking read on one handle never holds up another. Two threads
using the same handle are serialised by its lock, as MoarVM does. The
per-spawn walk that collected the handles a worker might reach, the
`ThreadPromisePayload` transfer and the handle clones are deleted.

`t/concurrency/concurrent-io-handle-table-shared.t` pins sibling workers'
handles, an array `await`, `.result`, a close and a read on a worker seen by
the opener, and handles published through a shared array, all matching
`raku`.
