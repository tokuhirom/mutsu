# A declaration's `where` runs once for its initializer

`my $x where { ... } = 5`, `my Int $i where { ... } = 3` and `my S $s = 1`
for a `subset S where { ... }` ran the predicate twice for the initializer:
once in the declaration's own `TypeCheck` and again in the store that
followed it. The fused `SetLocalDecl` now records that the declaration
already type-checked its value (`typechecked`), and the store skips its own
match, so a predicate with side effects runs exactly once per assignment, as
in Rakudo. A `where` block's reads of the declaring scope's lexicals at top
level, in a block and in a routine are pinned alongside (#10732).
