# `Supply.interval` is a class-method row

`Supply.interval`, `Compiler.id` on the type object and `X::AdHoc.payload` were answered by
by-name gates on the interpreter's slow path. `Supply.interval` is now a class-method row next to
`Supply.from-list`, a `Systemic` class's type object reaches its own type-object rows by owner, and
the payload gate is gone behind its row (ADR-11276 §9.196). A `Systemic` attribute now needs an
instance as in Rakudo, so `Compiler.name` on the type object raises instead of answering `Nil`.
