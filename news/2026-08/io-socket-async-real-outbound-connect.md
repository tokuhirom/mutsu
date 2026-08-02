# `IO::Socket::Async.connect` opens a real outbound TCP connection

`IO::Socket::Async.connect` used to consult *only* the in-process listener
registry. If no listener in the same interpreter owned the target address it did
not attempt anything — it simply broke the Promise:

```raku
# with any external server listening on 31417
await IO::Socket::Async.connect('127.0.0.1', 31417);
# was: Failed to connect to '127.0.0.1:31417'
```

So no mutsu program could talk to a server in another process, while
`IO::Socket::INET` (the synchronous socket) connected fine and
`IO::Socket::Async.listen` already bound a real `TcpListener`. The asynchronous
client half was the only simulated piece.

## Changes

- `dispatch_socket_async_connect` now falls back to a real
  `TcpStream::connect_timeout` when no in-process listener owns the address. The
  resulting socket is tagged `tcp-real` and registered with
  `register_tcp_stream`, which is exactly the shape the *accepted* side of a real
  connection already had — so `.Supply`, `.Supply(:bin)`, `.print`, `.write`,
  `.close` and `.native-descriptor` all work through the existing `tcp-real`
  paths in `socket_async_conn.rs` with no new plumbing.
- Every address the host resolves to is tried in turn, so `localhost` on a
  dual-stack box still connects when only one family has a listener.
- A failed connect now breaks the Promise with an `X::AdHoc`, not a bare `Str`:
  a consumer that catches it may `.rethrow` it, which Cro's pipeline QUIT handler
  does.
- `Tap.close` on a listener is now **synchronous**. The accept thread polls a
  closed flag every 10ms, so the OS socket stayed bound for a moment after the
  Tap was closed. That was invisible while `connect` was in-memory only (the
  registry entry vanished at once), but with a real connect a client could still
  connect to a just-closed listener. The accept thread now drops the listener and
  raises a `stopped` flag, and `set_listener_closed` waits for it (bounded at 2s
  so a wedged thread cannot hang the interpreter).

## Impact

This unblocks `Cro::HTTP::Client` — every Cro round-trip test drives it — and any
program that speaks to an external service over an async socket. The mutsu Cro
*server* already served real HTTP correctly (verified with `curl`); the client
half could not open a connection at all.

Pinned by `t/io-socket-async-real-connect.t`, which starts a helper listener in a
*separate process* (an in-process one would take the in-memory path), connects to
it, checks the socket has a real OS file descriptor, and round-trips bytes.

Two neighbouring defects surfaced while writing that test and are filed
separately:
`todo/tickets/supply-lines-drops-channel-backed-supplies.md` (`.lines` emits
nothing on a real-TCP Supply) and
`todo/tickets/procasync-stdout-is-not-incremental.md` (`Proc::Async` output
arrives only when the child exits, which is why the test hands the port over
through a file).
