# A role method on a mixin defers to `Mu`, not to itself

A role method that defers with `nextwith`/`callsame` out of a value the role is mixed
into now reaches the `Mu` base candidate, as on rakudo.

Before, a punned role's instance (`R.new`) wrapped an instance of the pun class `R`. Its
deferral candidates were read from that class's MRO, which answered the role's own method
again. So `method clone { nextwith(...) }` ran twice and then returned `Nil`. With
`K.new but R`, the inner class had no candidate, and the deferral returned `Nil` instead
of `Mu.clone`, `Mu.gist` or `Mu.Str`. A role that declared its own `clone` was also skipped
outright by the built-in mixin clone. Its `:attr(...)` overrides now coerce by sigil, so a
`Seq` passed to an `@` attribute becomes an `Array`.

Dan's `DataFrame.grep` calls `self.clone`, which `nextwith`s from the punned role. It died on
`.data` of `Any`. Dan's and Dan::Pandas' `t/03-dfo.t` now match raku.
