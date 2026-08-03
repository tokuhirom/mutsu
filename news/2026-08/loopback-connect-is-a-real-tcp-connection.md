# A loopback `IO::Socket::Async.connect` is a real TCP connection

`IO::Socket::Async.connect` used to consult an in-process listener registry
first: when the target host:port belonged to a listener in the *same* mutsu
process, it skipped the network entirely and handed back an **in-memory socket
pair** — two `Instance`s wired to each other through a global connection map,
with their own buffering, encoding and close plumbing.

That pair has no OS file descriptor, so `.native-descriptor` answered `-1`, and
any NativeCall consumer of it broke. Cro::TCP::NoDelay is exactly such a
consumer:

```raku
sub nodelay($socket) is export {
    my $nd = $socket.native-descriptor;
    ...
    if setsockopt($nd, PROTO_TCP, TCP_NODELAY, $on, $size) {
        my $errno := cglobal(Str, 'errno', int32);
        die "Failed to set TCP_NODELAY option on socket #$nd; errno = $errno";
    }
}
```

`setsockopt` on fd `-1` fails, so the `die` path ran — and it ran into a second
mutsu gap (`cglobal(Str, ...)`, which should resolve against the current
process, is treated as a library named `(Str)`), producing the bewildering
`Cannot locate native library 'lib(Str).so'`. Every Cro client that talks to a
server in its own process — which is every Cro test — died there before sending
a byte.

`.listen` has bound a **real** `TcpListener` with a real accept thread for a
while now, and the accepted side of such a connection is already driven by the
`tcp-real` paths. So the in-memory short-circuit was not a fallback for
something missing; it was a second, parallel implementation of a thing that
already worked. `connect` now always opens a real TCP stream, and a loopback
connection goes through the same OS plumbing as an outbound one. Both ends get
a real file descriptor.

With the short-circuit gone, the whole in-memory socket simulation became
unreachable and was deleted: the `AsyncSocketConnState` registry with its
`deferred_accept_*` handshake, the buffered in-memory `Supply` / `write` /
`close` implementations and their `AsyncSocketSupplyState` decoder, the
`lookup_async_listener` / `allocate_async_listen_port` helpers, and the
`callback_uses_supply_list` AST-debug-string sniff that decided whether an
accept had to be deferred. Net ~530 lines removed, and `IO::Socket::Async` has
one connection implementation instead of two.

Measured on the vendored Cro::Core suite, `t/tcp.rakutest` goes from 21 to 29
passing assertions, and the `Server connection with Listener options :nodelay`
subtest passes for the first time. On the Cro::HTTP side the client now
establishes its connection and sends its request; the response path is still
blocked elsewhere (see
`todo/deep/promise-of-an-on-demand-supply-ignores-nested-whenever.md`).

The two remaining `t/tcp.rakutest` failures are unrelated to the descriptor:
`Cannot connect to service after it has been stopped` (the OS listener outlives
a Cro service `.stop` briefly, so the next connect still succeeds) and
`Establishing connection dies before service is started` (a Cro composition
error, `Components controlled by a connection manager must compose to form a
transform or a sink`).
