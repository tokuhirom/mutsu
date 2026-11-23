# `$p[0] = v` on a Pair assigns the container, as for a scalar

A Pair is not Positional, so rakudo's `Any.ASSIGN-POS` treats it like any
other defined non-Positional value: `AT-POS(0)` is the value's own container.
`my $x = (a => 1); $x[0] = 9` now assigns the variable (it used to die with
`Cannot modify an immutable Nil value`), and `$x[1] = 9` is
`Index out of range. Is: 1, should be in 0..0`. The chained forms
`@a[0][0] = 9` over a Pair element and `%h<k>[0] = 7` over a Pair value replace
the element or entry (#18960), as #18952 did for scalars.

A store into a bare literal (`5[0] = 3`) and a walk three or more subscripts
deep still differ from rakudo; they stay open on #18960.
