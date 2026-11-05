# `IO::Socket::Async` reports numeric `socket-host` / `peer-host`

The client's `peer-host` and the accepted connection's `socket-host` were the
host name given to `connect` / `listen` (`localhost`); they are now the numeric
address of the connected socket (`getpeername` / `getsockname`), `127.0.0.1`, as
rakudo reports. This fixes the `socket-host works` assertion of
`IO::Socket::Async::SSL`'s `t/client-server.rakutest` (#16029).
