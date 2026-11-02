# A `.WHO<k>` store does not add a `$k` key

`Foo.WHO<D> = 4` and `Foo.WHO<D> := 4` put the entry `D` into the stash, and also left a `$D` scalar
key behind (`Foo.WHO.keys` was `($D D)`, Rakudo has `(D)`; a lowercase key came out as `$k` only). An
unsigiled key is now recorded as a term of the package, like a sigil-less `our constant`, so the stash
lists it under its own spelling and nothing else (#15965).
