# A chained store with a `[0]` tail over a defined value assigns it

`my @c = 1, 2; @c[0][0][0] = 9` died with "Cannot modify an immutable Int
(1)", where rakudo gives `[9 2]`: a defined scalar's (or Pair's) `AT-POS(0)`
is its own container, so every `[0]` over it addresses the same container
again. The 3+-level chained store now reads the chain first and, when it
reaches a defined scalar followed only by `[0]` levels, stores through the
shorter chain that the two-level rule already handles; another index there is
out of range `0..0`, as in rakudo (#18960).
