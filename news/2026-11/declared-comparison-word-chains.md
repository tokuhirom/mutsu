# A declared comparison word chains pairwise

After `multi infix:<eq>(...)` was declared, `"a" eq "a" eq "a"` folded left (`("a" eq "a") eq "a"`
compares `True` with `"a"`), so it answered `False` where rakudo answers `True` (#15659): a declared
word took the custom-infix path, which only knows `is assoc<chain>` as "one call over n operands". A
declared word the core grammar chains (`eq ne lt gt le ge eqv before after`, no `is assoc` trait) now
builds the same `Expr::ChainedCompare` the undeclared word does, so each adjacent pair dispatches through
the declared candidates and the core body and the shared middle operand is evaluated once. Pinned by
`t/vm/binding/declared-comparison-word-chains.t`.
