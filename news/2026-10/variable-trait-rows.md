# `Variable.block` and `Variable.var` are `Variable`'s rows

The reflective `Variable` a variable trait handler receives answered `.block` and `.var` through
two by-name gates on the interpreter's slow path. They are now rows of a `Variable` owner
(ADR-11276 §9.195), so `.^can` sees them as Rakudo's does, and `$x.VAR.var` is
`X::Method::NotFound` as in Rakudo instead of answering the variable.
