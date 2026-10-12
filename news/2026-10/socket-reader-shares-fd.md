# A tapped async socket costs one file descriptor, as in rakudo

Tapping an `IO::Socket::Async` connection's `.Supply` gave its reader a `dup` of
the socket. Every tapped end of a connection therefore held two descriptors,
where rakudo holds one. A Cro keep-alive client that opens a fresh connection
per request grew by four descriptors a request instead of two, and so reached
the descriptor limit twice as fast.

The read reactor (#12865) now polls the connection's own stream, which it
shares with the writers, and holds that stream open for as long as it watches
it. `close` still shuts the socket down, which the reactor reads as end of
stream. Only the portable reader, which runs one thread per connection and
blocks in `read`, keeps a private copy.

The test file's idle-tap check also became robust to a pool worker that
retires while it is being measured (#12981).
