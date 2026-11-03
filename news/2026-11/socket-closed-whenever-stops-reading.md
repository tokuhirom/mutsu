# Closing a `react` whenever on a connection's Supply stops its reader

A connection's read Supply reads the socket only while a tap listens, so a
consumer that is finished leaves the next chunk in the kernel buffer for the
next `.Supply` tap. A `react` `whenever $conn.Supply` closed with `Tap.close`
only marked the `whenever` closed: its subscription queue stayed live, the
reader thought somebody still listened, and it took the next chunk and dropped
it. `IO::Socket::Async::SSL.upgrade-server` after a plain-text exchange
therefore never saw the TLS ClientHello and `t/upgrade.rakutest` hung.

`close_whenever` now also closes the receiver queue of a competing (exclusive)
source bound to that `whenever` (`bind_whenever_close_flag`). With it
`t/upgrade.rakutest` is 3/3 like rakudo. Pinned by
`t/concurrency/supply/socket-retap-after-closed-whenever.t`.
