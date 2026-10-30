# A method family's declared-type relation is built once, not per dispatch

Ranking the candidates of a `multi method` call compared every pair of the
family (`RakuAST::Deparse` has 267 candidates, 71k pairs) to assign rakudo's
tiers, then read the tiers of the two or three candidates that bound the call.

Two changes, both exact:

- `assign_tiers_wanted` computes the tier of the candidates the caller reads
  (and of those in the trial-bind pool) as the longest chain of narrower
  candidates above them, walking predecessors lazily. The relation is cyclic
  only across an arity mismatch; the walk then gives up and the full relation
  is built as before.
- The declared-type part of the relation (a candidate that cannot bind the
  arguments is ordered by declared types alone) does not depend on the
  arguments, so `DeclaredIndex` holds it per family, keyed by the sorted
  `DeclaredKey` ids and dropped with the registry stamp, and a dispatch reads
  predecessor rows instead of comparing pairs.

Callgrind, six DEPARSE snippets per round (profiling build, steady state):
1,124M -> 880M Ir (-22%); warm wall 0.37-0.39 s -> 0.30-0.34 s per round
(rakudo 0.043-0.052 s).
