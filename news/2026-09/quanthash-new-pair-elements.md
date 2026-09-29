# Parameterized QuantHash `.new` checks Pair arguments as elements

`SetHash[Pair].new("a" => 0, "b" => 1)` (and `Set[Pair]`) unwrapped each Pair to its value before
type-checking it against the element type, so the check failed with "expected Pair but got Int".
`.new` takes its arguments as elements for every QuantHash, so a Pair is now checked as a whole,
matching `Bag`/`Mix` and Rakudo. Pinned by `t/routines/signature/quanthash-parameterized-pair-elements.t`.
