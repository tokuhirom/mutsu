# The subscript protocol of shapeless receivers is method rows

`Nil.AT-POS`, `Nil.AT-KEY`, a built-in type object's `AT-KEY`/`EXISTS-KEY` and the associative
subscript of a hash with no settled shape were the last name-matched arms of the pure
one-argument method cascade. They are now method-table rows: `Nil` declares its own `AT-POS` and
`AT-KEY`, as in Rakudo, and the others reach `Any`'s or their type's rows. `Mu.AT-KEY` is now "No
such method", as in Rakudo, since `Mu` is not an `Any` (ADR-11276 §9.154).
