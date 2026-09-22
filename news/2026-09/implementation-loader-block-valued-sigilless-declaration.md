# `Implementation::Loader` reaches green after a sigilless declaration parser fix

Locked and worked via the ecosystem distribution roulette (board:
[tokuhirom/mutsu#8977](https://github.com/tokuhirom/mutsu/issues/8977)).
`Implementation::Loader` 0.0.10 moved from `partial` (2/3 baseline files,
17/31 assertions) to `green` (3/3 files, 31/31 assertions).

Its dynamic loader uses a sigilless declaration whose value is a `do { ... }`
block immediately before a next-line `unless`. The parser lowered that
declaration to a synthetic block but did not recognize its block-final shape,
so it absorbed the conditional as a statement modifier. The loader then saw
an uninitialized role name and rejected a type that already did the role.

The parser now inspects the source-bearing statement inside synthetic
sigilless declarations when deciding whether a statement ends with a block,
including the case where declaration parsing has consumed the following
newline. A focused regression test covers the boundary under `t/control/`.
