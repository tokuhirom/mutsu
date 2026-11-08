# ADR-15519 accepted: mutsu will support the core `Awaitable::Handle` protocol

Maintainer decision (2026-11-08): the undocumented-protocol exception to the core-routine
documentation rule is granted for `Awaitable::Handle` only, after a check that rakudo has no
deprecation or redesign planned (it ships in 2026.09 for every language revision and the core
`await`, Promise, Channel and Supply coercers use it). Implementation follows the ADR's two
units: the role contract first (unblocking Async::Workers and OO::Plugin at load), then `await`
consuming the handle protocol. #15519 stays open for that work.
