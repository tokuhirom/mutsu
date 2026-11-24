# `@a[i] = Nil` stores the default when a closure captures `@a`

Once a closure captured an array or hash, assigning `Nil` to one of its
elements stored a literal `Nil` instead of the container's default
(`my @a = 1, 2; my $c = { @a }; @a[1] = Nil` gave `[1, Nil]`). A captured
aggregate lives in a shared cell, and the store-time default lookup read the
cell instead of the aggregate inside it, so it found neither an
`is default(...)` value nor the element type. It now looks through the cell
and answers `[1, Any]` like rakudo (#19109).
