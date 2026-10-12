# The socket ops of the Asynchronous `nqp::` family

`nqp::asynclisten`, `nqp::asyncconnect`, `nqp::asyncreadbytes` and
`nqp::asyncwritebytes` now work (#11502, ADR-11502 §2.4.4), and `nqp::closefh`
closes a socket or a child's stdin. With them all eleven Asynchronous ops are
implemented. Each op pushes `[schedulee, args...]` onto a user-declared
`is repr('ConcBlockingQueue')` queue, with the shapes Rakudo 2026.09 pushes:

- a listener reports once it listens, then once per accepted connection;
- a connect reports the connection or the error;
- a read reports a `Buf` per chunk, then the end of the stream;
- a write reports the bytes it wrote.

`nqp::asyncwritebytes` on a process handle from `nqp::spawnprocasync` writes
the child's stdin.

The listener bind, the accept loop and the connect moved out of
`IO::Socket::Async` into `runtime/tcp_core.rs`, and both now use them. The
read loop is the one the process pipes use.

Coverage: 569 / 575 (`docs/nqp-op-coverage.md`).
