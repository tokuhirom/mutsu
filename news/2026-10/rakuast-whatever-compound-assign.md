# RakuAST: `* *= 2` round-trips as a WhateverCode

`@a.map(* *= 2)` read back through RakuAST spells its `*` as a priming
argument, and the lowering expanded the compound assignment over that leaf
as an assignment to a read-only value ("Cannot modify an immutable value").
It now curries to the same `{ $_ *= 2 }` WhateverCode the parser builds, so
the round trip mutates the mapped array as rakudo 2026.09 does. Five more
`t/` files pass under `MUTSU_RAKUAST=1`.
