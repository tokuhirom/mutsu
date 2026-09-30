# The compiled regex engine runs `||`

ADR-0135's compiled regex engine (Slice A, #10251) now compiles sequential alternation. Before
this change, any pattern that contained `a || b` was declined and fell back to the tree walk.

The compiled form is laid out in the order `walk_seq_alternation` uses:

- Every way branch *k* can match is tried against the rest of the pattern before branch *k+1*
  is entered.
- Each branch ends with an `AltTail` op. It adds exactly what the walk's
  `alternation_branch_delta` adds: padding for the positional slots of the alternation's widest
  branch, and the list-valued names marked quantified. The padding is built by one function
  that the walk shares.
- Under ratchet the alternation commits to the first branch that matches, and to that branch's
  first end.

The walk moves on past a branch whose every end is zero-width. A ratcheted alternation with such
a branch before the last one is still declined, as is a numbered alias (`$0=`) inside a branch,
which the walk numbers from the branch's own start.

Comparing the two engines turned up a walk bug. The flag that turns off a quantified
alternation's padding stayed set while the walk ran the rest of the pattern after the loop. So in
`/ [ a || b ]+ [ (c) || d ] (x) /` on `"adx"`, `(x)` became `$0`, where rakudo numbers it
`$1`. The walk now clears the flag before running the continuation, and
`t/regex/match/regex-alternation-padding-after-quantified-loop.t` pins the rakudo numbering.
