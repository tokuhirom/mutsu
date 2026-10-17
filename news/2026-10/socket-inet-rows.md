# Socket methods are method-table rows

The instance methods of `IO::Socket::INET` and `IO::Socket::Async` are now rows of the one method table
(ADR-11276 §9.158). They replace three by-name `match`es and their hand-written name lists.

- `IO::Socket::INET`: `accept`, `print`, `put`, `write`, `recv`, `read`, `get`, `lines`, `nl-in`, `close`,
  `localport` and `getpeername`.
- `IO::Socket::Async`, for both TCP and UDP sockets: the address methods, `Supply`, `print`, `write`, `print-to`,
  `write-to`, `close` and `native-descriptor`.

As in Rakudo, `IO::Socket::INET` has no `say` method of its own.
