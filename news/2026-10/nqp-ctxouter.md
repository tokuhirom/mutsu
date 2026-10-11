# nqp::ctxouter, and a computed getlexouter that skips the running scope

`nqp::ctxouter($ctx)` now answers the context of the scope enclosing a frame, which completes the
Context Introspection category (#11498). A context is a flattened env snapshot with no scope
structure, so `nqp::ctx()` records the enclosing scopes when it runs (`Env::enclosing_scopes`, from
the frame root's static link): a closure's creating scope first, then the program scope, then an
empty outermost context whose `ctxouter` is null.

A computed `nqp::getlexouter($name)` walks the same scopes, so a binding of the running routine
that shadows the outer one is no longer returned.

Known gap: a closure's capture holds only the names it uses, so the creating scope's pad lacks the
lexicals the closure never mentions (Rakudo keeps the whole frame).
