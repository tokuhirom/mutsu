# A `Junction:D` parameter binds a Junction whole when a closure is called by value

`find_junction_autothread_arg_with_pointy` compared a parameter's constraint with `"Junction"` /
`"Mu"` without stripping the `:D` / `:U` smiley, so `-> Junction:D $j { ... }` called as `f(1|2)`
autothreaded the Junction and failed with `expected Junction but got Int (1)`. This hit every
multi candidate of an imported `proto`/`multi` family (they are called through the captured-candidate
path), such as `eigenstates(1|2|3)`. The smiley is now stripped first, as the by-name call path already did.
