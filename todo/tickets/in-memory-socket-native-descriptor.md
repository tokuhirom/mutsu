# In-memory async sockets have no native-descriptor (blocks Cro::TCP :nodelay path)

`IO::Socket::Async.connect` to an in-process listener creates an **in-memory**
socket pair (`dispatch_socket_async_connect`), not a real TCP connection. Such a
socket has no OS file descriptor, so `.native-descriptor` is unimplemented.
Cro::TCP::NoDelay's `nodelay($socket)` calls
`$socket.native-descriptor` and then NativeCall `setsockopt(...)` on it, so
`Cro::TCP::Listener.new(:nodelay)`'s accept path dies inside the `whenever`
body (the error is swallowed by the listener-callback dispatch) and
`t/tcp.rakutest` subtest "Server connection with Listener options :nodelay"
hangs waiting for the connection that was never emitted.

Repro (with Cro::Core's lib):

```raku
use Cro::TCP;
my $lis = Cro::TCP::Listener.new(port => 31313, :nodelay);
my $conns = Channel.new;
my $tap = $lis.incoming.tap({ $conns.send($_) });
my $client = await IO::Socket::Async.connect('localhost', 31313);
my $sc = $conns.receive;   # hangs: whenever body died in nodelay($socket)
```

Options considered:

- Returning a fake fd (-1 or a dummy) makes `setsockopt` fail with EBADF and the
  Cro code `die`s — worse than missing.
- Backing each in-memory pair with a real `socketpair(2)` just to have valid
  fds would satisfy `setsockopt`, but the data still flows through the
  in-memory channels, so the fd is a lie with real cost.
- The architecturally honest fix is to make localhost `connect` use the real
  `TcpListener` (real accept, real fds, real byte streams) and reserve the
  in-memory simulation for targets without sockets (wasm). That is a sizable
  campaign: the in-memory supply/write/close plumbing in
  `socket_async_conn.rs` is load-bearing for many S17/io tests today.

Affected: `tmp`-vendored Cro::Core `t/tcp.rakutest` subtest 3+ (`:nodelay`),
any real NativeCall use of `.native-descriptor` on async sockets.
