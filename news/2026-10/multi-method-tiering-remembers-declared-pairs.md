# Multi method tiering remembers the declared-type pair relation

`assign_tiers` orders every candidate of a multi method (matched or not, #12795) by rakudo's
topological sort, which is O(n^2) pair comparisons. Rakudo's `RakuAST::Deparse` has 267
`multi method deparse` candidates, so each dispatch with a new argument-type key compared ~71,000
pairs. Almost all of them are candidates that cannot bind the call, which `declared_narrower`
orders by their DECLARED types alone, independent of the arguments.

A pair of such candidates is now remembered under the two candidates' `DeclaredKey`s (arity,
slurpiness, invocant, declared positional types and their unrefined bits: exactly what
`declared_narrower` reads). Like the existing tier memo, only a pair whose `conforms` answers were
all static (measured RakuAST names, `Mu`, `Any`) is remembered; a pair that read the registry or
the env is recomputed every time, so no invalidation is needed.

Measured with callgrind on six `DEPARSE` snippets per round (profiling build, steady state =
(3 rounds - 1 round) / 2, both binaries built from the same `main`): 3,556M -> 2,634M Ir per round
(-26%); `declared_narrower` calls over three rounds 7,455,964 -> 70,490; the whole tiering pass
28% -> 5.7% of the run. Wall clock moved less (0.92 s -> 0.84 s per round of six snippets, rakudo
0.065 s): the next term is the per-candidate argument matching (`args_match_param_types_inner`,
about half of what is left), a separate slice of #15641.
