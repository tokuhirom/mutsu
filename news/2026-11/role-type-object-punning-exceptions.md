# A role type object keeps rakudo's `Mu` methods and puns the rest

A method call on a role type object puns the role, except for the fixed set
of names rakudo's `RolePunning` keeps on `Mu` (`ACCEPTS`, `item`, `Bool`,
`gist`, `raku`, `perl`, `Str`, `Numeric`, `Real`, `WHICH`, `WHY`, `set_why`,
...). mutsu kept only `Str`/`gist`/`raku`, and only on its punning entries:
the compiled method resolver still found the role's own `ACCEPTS` or `Str`
on the type object. Conversely `R.list`, `R.hash` and `R.elems` were answered
by the built-in method instead of the role's (#18062).

The kept set is now rakudo's, one list (`role_type_object_keeps_mu_method`)
consulted by the punning entries, the compiled resolver and the interpreter
path, and a role type object whose role declares a method shadows the
built-in of that name.
