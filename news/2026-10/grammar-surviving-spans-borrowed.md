# Grammar.parse no longer builds an owned span set of the whole match tree

The #12383 fix for actions of backtracked alternatives computed the set of
surviving `(rule, from, to)` spans on every `Grammar.parse`, allocating a
`String` per node even when nothing needed it. The set is now built only when
the reduced-subrule log is non-empty, borrows the rule names from the tree and
uses a fast hasher. `bench-yaml-parse` is back under its pre-#12383 instruction
and allocation counts (352.9M / 398k vs 357.1M / 416k).
