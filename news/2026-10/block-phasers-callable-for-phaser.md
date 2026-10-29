# `Block.phasers` and `Block.callable_for_phaser`

A `Block` now answers `phasers(name)` (the block's phasers of that name, as a `List` of `Block`s, in
the order rakudo runs them) and `callable_for_phaser(name)` (one `Block` that runs them all, or `Nil`
when there are none). Parallel-iteration modules such as ParaSeq use them to pull the `FIRST`,
`NEXT` and `LAST` phasers out of a block they run themselves (#15375). A `WhateverCode` has neither,
as in rakudo. A phaser's assignment to a captured scalar is not written through yet (#15473).
