# An imported multi family stops re-resolving on every call

A `proto`/`multi` family imported from a module is compunit-scoped
(#11004): which candidates a call may see depends on the unit asking. For
that reason two caches had left such families out entirely, so every call
paid the full cost again:

- the trivial-proto winner memo;
- the visibility check that `has_multi_candidates` / `has_multi_function`
  run, which walked every candidate key of the family.

Both are now keyed by the units that decide visibility: the current unit, the
executing unit and the operator-import generation. That is how the candidate
memo already handled these families. Both caches are only trusted while the
functions map and the protos stay exactly as they were.

Array::Sorted::Util's `inserts`/`finds` are such families. A 300-call
`inserts` loop drops from 457M to 348M instructions (#15786).
