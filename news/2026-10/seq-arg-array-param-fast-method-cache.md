# A Seq argument to a method's `@` parameter survives repeated calls

The cached fast method-call path bound a `Seq` argument to an `@` parameter as
the raw Seq, while the full binder binds it through `.cache` (a re-readable
`List`). Only the first call on a receiver took the full binder, so from the
second call on `@c.map(...)` inside the method saw a spent Seq and returned
nothing (`%rig<server>.execute-tool-calls((^3).map(...))` in MCP::Server).
A call that passes a `Seq`/`LazyList` to a method with a non-slurpy `@`
parameter now declines the fast paths and uses the full binder.
