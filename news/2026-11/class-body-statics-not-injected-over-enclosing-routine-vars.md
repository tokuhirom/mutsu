# A class-scoped sub no longer gets the class-body `my` injected over its enclosing routine's variable

Calling a named sub declared inside a method (or a `supply` block in one) injected
the class body's same-named `my` into the call env, shadowing the enclosing
routine's own parameter or local. The injection now skips the names
`CompiledCode::routine_scoped_free` lists. This fixes the remaining
`IO::Socket::Async::SSL` server-side ALPN difference (#16077).
