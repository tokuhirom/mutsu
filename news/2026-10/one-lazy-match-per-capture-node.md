# One lazy Match per capture node: wrapped grammar recursion is linear again

A lazy `Match` memoized its materialization per `Match` value, but every parent that
materialized a capture node minted a fresh `Match` for it. A `.wrap`ped self-recursive token
sees each level both as the wrapper's `callsame` result and as `$<TOP>` of the level above, so
Grammar::Extractor, which keeps and renders every level's result, forced the whole subtree again
at each level: N²/2 materialized nodes for a recursion N deep, and 3.5 GB of peak RSS on its own
`t/01-basic`.

ADR-11404 gives a capture node one canonical lazy `Match` per engine run, through a weak back
edge from the node to the `Match` that presents it. Every parent and every slot that exposes the
node hands out the same `Match` (one instance id, one memoized materialization), which also
subsumes the per-materialization table that kept an alias and its rule name identical (GH #8167).
A wrapped subrule call now matches against the caller's subject instead of copying it into a new
`MatchTarget` per level, and a rule's lone candidate is no longer LTM-measured before the call
that runs it anyway.

Grammar::Extractor's `t/01-basic` (release build) went from 3.5 GB peak RSS to ~150 MB and
finishes within 2x of rakudo; see #11404 for the measured numbers. The remaining identity gap --
rakudo files the wrapper's returned `Match` itself as `$<TOP>` -- is #12918.
