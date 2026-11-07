# `=:=` on sigilless parameters and through a code object

`sub f(\p, \q) { p =:= q }` compared the parameters' values, so two distinct
variables holding `3` answered `True`: a sigilless parameter is now a named
operand like a `$` variable, resolved through the alias the binder records for
the caller's variable. `my &f = &[=:=]; f($a, $b)` (and a callable passed on to
another routine) likewise took its arguments by value; the call site's argument
sources now decide whether the two variables share a container. The output
matches raku for these forms; Iter::Able's `:with(&[=:=])` over array-iterator
elements still needs the iterator to hand out element containers (#16948).
