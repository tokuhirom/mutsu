# A `where` clause no longer outranks a subset in multi dispatch

Multi-sub ranking counted `where`-constrained parameters and subset-typed
parameters as two separate components of the refinement tier, compared in that
order. So `multi f(Int $x where * < 10_000)` beat `multi f(Small $x)` (with
`subset Small of Int where * < 100`) for `f(5)`, whichever was declared first.

Rakudo draws no line between the two: each is a bind-time constraint on top of
the same nominal type. A constrained parameter is narrower than an unconstrained
one of that type, and two candidates that differ only in which kind of constraint
they use tie, so declaration order decides. The refinement tier now counts
*constrained parameters*, a `where` or a subset counting once per parameter, which
is what method dispatch already did. The mismatch showed up as a wrong checksum
in the new `benchmarks/bench-multi-dispatch.raku`.
