# `.map` over a Block with a role mixed in keeps the Block's arity

`(1..6).map(-> $a, $b { ... } but R)` called the mixed-in Block with one
element at a time and died with "Too few positionals", because the map and grep
loops counted a callback's parameters only for a plain Block. A role mixed into
the Block is now looked through: the elements are taken a group at a time, as
the Block's signature asks. ParaSeq mixes its `BlockMapper` role into every
user block, so its degree > 1 `map(-> $a, $b { $a })` returned nothing.
