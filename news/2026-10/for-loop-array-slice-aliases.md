# A `for` loop over an array slice aliases the selected elements

`for @a[1..*-1] <-> $t { $t = ... }`, `for @a[1, 2] { $_ *= 2 }` and
`for @a[...] -> $t is rw` now write back into the array, as in raku. The
slice handed the loop copies of the elements, so the writes were lost.
Doc::Executable's parser relies on this to strip a comment prefix from every
code block after the first, and its whole suite now passes.

The loop now asks the slice for the elements' containers, the same request
`my @s := @a[1, 2]` makes. A bound slice also gained range indices with a
`*` or WhateverCode endpoint (`my @s := @a[1..*-1]`), which were refused as
an immutable List.
