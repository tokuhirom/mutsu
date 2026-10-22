# An imported routine's `where` clause runs at the call

A plain routine imported by a block-scoped `use` was called as if an earlier candidate
match had already vouched for its `where` clauses, so `where` constraints were skipped:
`benford-dist(2)` of `Statistics::Distributions::Utilities` (`UInt:D $b where $b > 2`)
returned instead of failing the bind. The call now evaluates the clauses itself;
only a winner picked by multi dispatch keeps the trusted verdict. Found running
`Statistics::Distributions`, whose `t/02-random-variate.rakutest` now matches rakudo.
