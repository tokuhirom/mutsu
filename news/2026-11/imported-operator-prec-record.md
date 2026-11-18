# An imported operator's precedence record reaches the importer

An operator a module exports with `is tighter` / `is looser` / `is equiv`
now carries its `Routine.prec` record into the unit that `use`s it, read from
the loaded module (ADR-17558 slice 3). An operator the importer declares
`is equiv(&infix:<imported>)` takes the imported operator's level instead of
the built-in one it was derived from (`u@=`, not `t=`), and a `multi`
candidate of it inherits it.

`is equiv` also no longer copies the associativity of the operator it names:
like `is tighter` and `is looser`, it leaves the new operator `left`-associative
unless `is assoc` says otherwise, as rakudo does.
