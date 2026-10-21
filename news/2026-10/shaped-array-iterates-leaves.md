# A multi-dimensional shaped Array iterates its leaves

In Rakudo, a shaped Array such as `my @c[2;2]` iterates its leaves in
row-major order, and its `.list` is the array itself. mutsu used to iterate
the first-dimension rows instead, so `.say for @c` printed `[1 2]` and
`[3 4]`. Its `.list` was a flat List. A `for` loop that wrote its topic back
stored a leaf over a whole row and changed the array's shape.

Iteration now walks the leaves everywhere it goes through the list
expansion: `for`, `.map`, `.grep`, reductions, `.join`, `.Seq` and `|@c`.
The list methods that iterate their receiver also see the leaves: `.sum`,
`.max`, `.unique`, `.first`, `.Bag` and the rest. `.list` is the array
itself, except that a native shaped array still lists as a flat List, as in
Rakudo. A topic or rw-parameter write in a `for` loop lands on its own leaf
(#14248).
