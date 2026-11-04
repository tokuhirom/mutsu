# Regex quoted-atom blocks naming a user type compile after `sub EXPORT` imports

A type name inside the `{ ... }` block of a quoted atom of a regex literal
(`/"{ Q.^name }"/`) panicked with `Expr::Spelled reached the compiler` in a unit
that imports a module declaring `sub EXPORT`. The compile-time qq thunk of the
atom is parsed straight from text and never went through the lowering's
spelling-stripping pass; it now strips spellings itself
(`ast::spelled::strip_spelling`). ValueClass's `t/01-basic.rakutest` now matches
rakudo (58/58).
