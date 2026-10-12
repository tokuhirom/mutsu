# `nqp::spawnprocasync`, `nqp::permit` and `nqp::killprocasync`

The process ops of the Asynchronous `nqp::` family now work (#11502, ADR-11502
§2.4.3). `nqp::spawnprocasync` spawns a child and answers a `BOOTIO` handle.
The child's events go onto a user-declared `is repr('ConcBlockingQueue')`
queue as `[callback, args...]`:

- `ready` with the stream fds and the pid;
- a `Buf` of the requested `buf_type` per output chunk, then the end of each
  stream;
- `done` with the raw wait status;
- or `error` when the spawn fails, with libuv's message and error code.

A stream is read only once `nqp::permit` allows it. The permit can be a
number of chunks, -1 for all of them, or 0 to pause. `nqp::killprocasync`
signals the child. Every shape was checked against Rakudo 2026.09.

The ops do not get a process layer of their own. Building and spawning the
child, and the pipe read loop, moved out of `Proc::Async.start` into
`runtime/proc_spawn.rs`, and both now use it. The two copies of the read
loop in `Proc::Async.start` became one. Merged output, a pty, and binding the
child's stdio to a given fd are refused with an error that names the option.

Coverage: 565 / 575 (`docs/nqp-op-coverage.md`).
