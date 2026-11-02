# A nested `sub` sees its enclosing method's variable, not the class-body `my`

A named `sub` nested in a method (or in a `supply` block inside one) read a free
variable from the class body's same-named `my` instead of the method's own
parameter or local, because the package store was consulted before the enclosing
routine's binding. `CompiledCode::routine_scoped_free` now records the free
variables an enclosing routine declares, and those skip the package store.
This fixes `IO::Socket::Async::SSL`'s server-side ALPN selector, which read the
module-level `my $alpn` (a `Bool`) and so rejected every handshake (#15986).
