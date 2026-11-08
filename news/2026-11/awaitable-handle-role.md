# The core `Awaitable::Handle` role exists

`Awaitable::Handle`, the role rakudo's `await` protocol returns from `get-await-handle`, is now a
real role (ADR-15519, first unit): the `$!already`/`$!success`/`$!result`/`$!cause` attributes with
their accessors, the `already-success` and `already-failure` factories, and the required
`subscribe-awaiter`, whose omission is a compile error as in rakudo. It was previously an
`Invalid typename`, which stopped Async::Workers and OO::Plugin from loading. Making `await`
consume the handle protocol is the ADR's second unit.
