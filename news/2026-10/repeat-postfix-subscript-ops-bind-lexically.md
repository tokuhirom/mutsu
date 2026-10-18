# Imported `x`, `xx`, `i` and subscript candidates stay in their block

A block-scoped `use` of a module that exports `multi infix:<x>`,
`multi infix:<xx>`, `multi postfix:<i>` or `multi postcircumfix:<[ ]>` /
`postcircumfix:<{ }>` candidates now adds them to the operator inside that
block only (ADR-8751 stage 2, slice 8). A routine written outside the block no
longer sees them when the block calls it, and a closure taken in the block
keeps them after the block ends; before, the import aliased them into the
routine registry, so it was the other way round. What none of the candidates
accepts is still the built-in operator's: `'ab' x 2`, `3\i` and `(1, 2, 3)[1]`
work as before in such a block.

Two fixes came along: `$s x= $r` now reaches a user `infix:<x>` candidate (it
ignored even one the unit declares), and an object used as a repeat count
coerces with `.Int`, as in rakudo, instead of repeating zero times.
