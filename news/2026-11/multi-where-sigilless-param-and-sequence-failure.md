# A multi candidate's `where` sees an earlier sigilless parameter

`multi S2 (Int \n, Int \k where * == n)` picked the wrong candidate: while the
dispatcher type-checked a candidate it bound each earlier parameter under its
plain name, but a sigilless parameter lives under its term key, so the
`where` clause read `n` as an undefined value. It warned "Use of
uninitialized value of type Any in numeric context" and compared against `0`.
The earlier parameters are now bound under their storage name (#19344,
found through Math::Sequences' `Sterling2`, whose `t/OEIS.rakutest` printed
about 66 such warnings).

Also from #19344: a sequence generator that answers a `Failure`
(`0, 1, 1, &NOSEQ ... *` with `sub NOSEQ { fail "..." }`) throws it when the
sequence reaches that element, as rakudo does, instead of caching the Failure
as an element that every later read trips over. Slices and range reads now
throw like the single-element read already did. mutsu throws the original
error on a second read too, where rakudo reports "This continuation has
already been invoked" (see `docs/rakudo-divergences.md`, C8).
