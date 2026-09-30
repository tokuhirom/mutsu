# Expression-position typed scalars keep definitive assignment checks

A later `Nil` assignment to a scalar declared as `(my Int:D $x = 1)` now raises
`X::TypeCheck::Assignment`, matching the statement-position declaration. The
expression declaration already registered its constraint; the by-name VM store
was resetting `Nil` to the nominal type object without checking `:D`.
