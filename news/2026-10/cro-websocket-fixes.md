# Transitive package-qualified enum values, `@a[a^..^b]`, `Supply.Channel.Supply`

Three gaps found by the ecosystem roulette on Cro::WebSocket:

- A package-qualified value of an `our` enum (`Cro::WebSocket::Message::Ping`,
  declared as `package Cro::WebSocket::Message { enum Opcode is export (...) }`)
  is global, so it resolves in any file that loaded the module transitively.
  The compile-time module scan now records the package-qualified forms and
  keeps them when they arrive through an intermediate module, so
  `when Cro::WebSocket::Message::Ping { ... }` no longer fails with "needs
  parens to avoid gobbling block".
- Array subscripts with an exclusive-start Range (`@a[2^..^5]`, `@a[2^..5]`)
  returned Nil; they now address the same window as `@a[3..^5]` / `@a[3..5]`.
- A value emitted after `$supplier.Supply.Channel.Supply` was tapped never
  reached the tap: the `Supply.Channel` forwarder only queued it on the
  channel. It now delivers through `Channel.send`'s own path.

`t/websocket-handler` and `t/websocket-message-parser` now pass under mutsu;
`t/websocket-message-serializer` needs #11238 (an array pushed by a closure on
a worker thread is invisible to a named sub capturing the same array).
