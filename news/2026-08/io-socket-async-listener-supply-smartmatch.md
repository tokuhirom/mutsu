# `IO::Socket::Async.listen(...)` now smartmatches `Supply`

`IO::Socket::Async::SSL`'s own upstream test suite (bundled as a `Cro::TLS`
dependency) failed broadly:

```
$ mutsu -I modules/IO-Socket-Async-SSL/lib -I modules/OpenSSL/lib t/ciphers.rakutest
No such method 'Supply' for invocant of type 'IO::Socket::Async::Listener'
  in sub BUILD at t/ciphers.rakutest line 565
```

Same error in `client-server.rakutest`, `bad-incoming.rakutest`,
`dh-ciphers.rakutest`, `ecdh-ciphers.rakutest`, `encoding.rakutest`,
`thread-stress.rakutest`, `upgrade.rakutest` — every file that actually opens
a listening socket and taps its connection stream.

## Root cause

Real raku's `IO::Socket::Async.listen(...)` literally IS a `Supply` (built
from a `supply { ... }` block in CORE.setting) — there is no separate
"Listener" type at all. mutsu implements it as a bespoke native object
(`IO::Socket::Async::Listener`, its own accept-loop) whose MRO was just
`["IO::Socket::Async::Listener"]`, so `$listener ~~ Supply` was `False`.
`IO::Socket::Async::SSL`'s `!server-setup` branches on exactly that
smartmatch (`if $connection-source ~~ Supply { whenever $connection-source
-> $sock {...} } else { handle-connection($connection-source) }`) — when it
failed, the listener object itself was treated as a single accepted
connection, and building an `IO::Socket::Async::SSL` instance around it
tried to call `.Supply` on the listener, which doesn't have that method.

## Fix, and a hidden second bug it uncovered

Adding `Supply` to the Listener's MRO looked like the whole fix, but it
introduced a hang: `native_methods/mod.rs` has TWO separate hardcoded
per-class dispatch tables (immutable and mutable native methods), each with
its own MRO-walk fallback for classes not directly listed. The immutable
table already explicitly listed `IO::Socket::Async::Listener`, so it was
unaffected — but the MUTABLE table's list did not include it, so its
MRO-walk fallback now matched the newly-added `Supply` ancestor and routed
`tap`/`act` calls to the generic (non-functional, no real socket behind it)
`native_supply_mut` handler instead of falling through to the Listener's own
real handler — creating a `Tap` whose `socket-port` Promise was never kept,
hanging any `await $tap.socket-port`. Root-caused with `rust-gdb`: neither of
the two known `tap`/`act` handler functions was ever hit, which pointed at a
third, previously-unexamined dispatch table.

Fixed by also explicitly listing `IO::Socket::Async::Listener` in the mutable
table's hardcoded list (mapping to its existing `_ => Err(...)` fallback, the
same behavior it already had before the MRO widening), so the class's MRO
can safely gain ancestors purely for `~~`/smartmatch purposes without any
hardcoded per-class native-method dispatch table silently rerouting through
an unrelated ancestor's generic handler.

With this fix, `IO::Socket::Async::SSL`'s `bad-incoming.rakutest` now passes
fully (3/3), `ciphers.rakutest` passes its first test (was failing with the
`.Supply` error before, now fails a second, unrelated TLS-negotiation test),
and the other previously-`.Supply`-erroring files progress further before
hitting their own separate (TLS-specific) issues.

New test: `t/io-socket-async-listener-is-supply.t`.
