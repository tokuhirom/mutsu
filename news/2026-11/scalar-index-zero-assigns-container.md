# `$x[0] = v` on a scalar assigns the scalar, as on rakudo

A positional store into a variable or element holding a defined scalar used to
die with `Cannot modify an immutable Int`. rakudo's `Any.ASSIGN-POS` stores
through `AT-POS`, and a defined scalar's `AT-POS(0)` is its own container. So
index 0 assigns the variable (`my $x = 5; $x[0] = 3; say $x` prints `3`, with
the variable's type check applied), and any other index is
`Index out of range. Is: 1, should be in 0..0` (#18952).

The chained forms follow the same rule: `@a[0][0] = 3` replaces the element,
and `%h<a>[0] = 9` replaces the entry. A Pair element, a walk three or more
levels deep, and a bare literal (`5[0] = 3`) still differ; #18960 tracks them.
