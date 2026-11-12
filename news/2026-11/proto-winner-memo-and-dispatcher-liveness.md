# Calls through a trivial proto stop re-ranking their candidates

Array::Sorted::Util's `inserts`/`finds`/`insert` are `proto` families, called
with named arguments (`:&cmp`, `:force`). Concurrent::PriorityQueue's
`t/02-concurrent` makes 32000 `inserts` calls, and a profile of that loop
showed three costs on every call, none of which depended on anything that
changed between calls:

- **The proto winner was resolved from scratch.** A call through a trivial
  proto (`proto sub f(|) {*}`) ranked every candidate again and cloned the
  winning `FunctionDef`. The winner is now remembered for the argument types,
  for as long as the functions map and the protos stay exactly the same. A
  family whose winner depends on argument *values* (a `where` or a subset
  type) is still decided per call. A block-scoped `proto` drops the memo, and
  so does anything else that changes the generation.
- **Every call through an imported `&name` dispatcher rebuilt the family's
  candidate list** to check that its captured candidates were still live. It
  now reads the existing per-generation candidate memo.
- **The binder built up to eight candidate names per parameter** for every
  named argument (`format!(":{}", key)`, `format!("@{}", key)`, ...) to see
  whether some parameter takes it. The check now compares prefixes without
  allocating.

Together these take about 15-20% off the `inserts` loop (release build).
Issue #15786 records the rest of the gap.
