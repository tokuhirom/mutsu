# The compiled regex engine runs loops whose body can match empty

ADR-0135's compiled regex engine (Slice A, #10251) used to decline any quantifier whose body
could match the empty string, such as `[ a? ]*`, `( a? a? ) ** 2..3` or `[ \s* \w ]+?`. It now
compiles them.

Each iteration of such a loop ends with a new `ZeroIter` guard. An iteration that consumed
nothing is accepted only while the walk's own `zero_width_iter_counts` says it still counts:

- a bounded quantifier counts it up to its maximum;
- an unbounded one counts it only while the count is below the minimum.

When the guard rejects an iteration, the engine backtracks into the body's other candidates.
That is exactly the walk's group DFS (`walk_quant_group_candidates`). The walk's other loop,
the chain, only ever takes an iteration's first candidate. So a nullable body compiles only when
it is a group or contains an alternation (the DFS shapes), or when it is ratcheted, or when it is
a zero-width assertion with a single candidate. In those last two cases the rejection simply
ends the loop, as it does in the chain.
