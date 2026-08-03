# Closing a listen Tap no longer throws away the backlog

`IO::Socket::Async.listen`'s accept thread polls a non-blocking `TcpListener`
with a 10ms sleep between tries, and it checked its close flag *before* each
`accept()`:

```rust
loop {
    if closed_flag.load(Ordering::SeqCst) {
        let _ = tx.send(SupplyEvent::Done);
        break;
    }
    match tcp_listener.accept() { ... }
}
```

The OS completes a TCP handshake without this thread's help, so by the time
`connect` returns on the client the connection is ESTABLISHED and queued in the
listener's backlog — it is waiting to be handed over, not to be made. A
`Tap.close` that landed inside the 10ms window therefore discarded a connection
the client believed it had, along with everything the client had already sent.

That is not a hypothetical: it is what made `roast/S32-io/IO-Socket-Async.t`
hang in **6 of 8 runs** once the in-memory loopback pair was removed (see
`loopback-connect-is-a-real-tcp-connection.md`). The test's last block connects
twice, writes to both, closes both listen taps and then waits for each server
body's `$conn.Supply.list`; with the accept dropped, `await $first-done,
$second-done` had nothing to wait for. Before the loopback change this shape
never touched the accept thread at all — an in-process `connect` was short-
circuited to an in-memory pair — so the race had simply never been reachable.

Seeing the close flag now switches the thread into a **draining** state instead
of stopping it: it keeps accepting until the backlog reports `WouldBlock`, and
only then sends `Done` and drops the listener. New connections still cannot be
made once the flag is set (`Tap.close` waits for the acknowledgement before
returning, so the port is free when it does), but a handshake that already
completed is delivered.

Pin: `t/io-socket-async-listen-tap-close-backlog.t`, which closes the tap with
the connection still only in the backlog and checks both that the accept fires
and that the bytes arrive. It passes under real `raku` too.
