# IO::Socket::INET methods are method-table rows

The instance methods of `IO::Socket::INET` (`accept`, `print`, `put`, `write`, `recv`, `read`, `get`, `lines`,
`nl-in`, `close`, `localport`, `getpeername`) are now rows of the one method table (ADR-11276 §9.158), replacing a
by-name `match` and its hand-written name list. As in Rakudo, the socket has no `say` method of its own.
