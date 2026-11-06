# `push @a, :x(1)` raises `X::Multi::NoMatch` like rakudo

The sub forms of `push`/`unshift`/`append`/`prepend` take no named argument. mutsu died with a
compile-time `die "Unexpected named argument ..."` (`X::AdHoc`); rakudo raises `X::Multi::NoMatch`
naming the call with the argument types it got, followed by its two candidates
(`(\a, \b)`, `(\a, **@b is raw)`). The call now evaluates its arguments and raises that exception
at run time. `multi_call_profile` also names a list-literal value of a Pair `List` instead of `Array`.
Found by the `Type/Hash.rakudoc` / `independent-routines.rakudoc` doc-diff sweep.
