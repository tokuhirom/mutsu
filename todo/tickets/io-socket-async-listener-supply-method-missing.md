# `IO::Socket::Async::Listener` has no `.Supply` method

## Symptom

`IO::Socket::Async::SSL`'s own upstream test suite (bundled as a
`Cro::TLS` dependency, see `docs/batteries/cro-deps.md`) fails broadly:

```
$ mutsu -I modules/IO-Socket-Async-SSL/lib -I modules/OpenSSL/lib t/ciphers.rakutest
No such method 'Supply' for invocant of type 'IO::Socket::Async::Listener'
  in sub BUILD at t/ciphers.rakutest line 565
```

Same error in `client-server.rakutest`, `bad-incoming.rakutest`,
`dh-ciphers.rakutest`, `ecdh-ciphers.rakutest`, `encoding.rakutest`,
`thread-stress.rakutest`, `upgrade.rakutest` — every file that actually
opens a listening socket and taps its connection stream.

## Why this does not block the Cro battery

`Cro::HTTP`'s own upstream suite (35/35) does not exercise a real listening
socket in its unit tests (it drives `Cro::Transform` pipelines directly via
`Supplier`, not `Cro::HTTP::Server.start`), so this gap does not surface
there. It WOULD surface for a real deployed Cro server using TLS/HTTP2 —
worth fixing before advertising HTTPS serving as a working use case.

## Root cause (not yet diagnosed)

`IO::Socket::Async.listen(...)` presumably returns a `Supply` of
`IO::Socket::Async` connections in real Raku (`raku -e 'IO::Socket::Async.listen("0.0.0.0", 0).WHAT.say'`
— verify). mutsu's `IO::Socket::Async::Listener` type (native, see
`src/runtime/native_socket*.rs` or similar) is apparently missing the
`.Supply` accessor/method entirely.

## Next steps

1. Confirm expected shape with `raku`: what does
   `IO::Socket::Async.listen(...)` return, and what does `.Supply` do on it?
2. Find mutsu's native `IO::Socket::Async::Listener` implementation and add
   the missing method.
3. Re-run `IO::Socket::Async::SSL`'s suite; the 8 currently-failing files
   should pass once this lands (verify each individually — there may be
   secondary gaps behind this one).

## Reproduce

```sh
git clone https://github.com/raku-community-modules/IO-Socket-Async-SSL.git /tmp/ioasssl
cd /tmp/ioasssl && git checkout 0.8.2
timeout 20 mutsu -I /path/to/mutsu/modules/IO-Socket-Async-SSL/lib -I /path/to/mutsu/modules/OpenSSL/lib t/ciphers.rakutest
```
