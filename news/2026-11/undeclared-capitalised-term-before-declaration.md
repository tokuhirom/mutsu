# A capitalised name with no setting meaning is `Undeclared routine` before its term is declared

`say FOO; constant FOO = 3` and `say Y; enum E <Y Z>` are rakudo's compile-time
`Undeclared routine`: routines are hoisted, terms are not, and the setting has no
`FOO` or `Y`. mutsu now raises it in `Finalize` (the in-order finalizer) when the
bareword names a capitalised term that a later statement of an enclosing list declares
and neither the unit's types nor the setting (known types, core terms, `X`,
`Metamodel`) have the name; a name the setting does have still resolves to the
setting's (`say X; constant X = 3` prints `(X)`, ADR-15760 slice 1d).
Closes #16168.
