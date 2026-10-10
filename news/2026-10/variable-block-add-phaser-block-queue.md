# Variable.block.add_phaser runs in the block's own phaser queues

A phaser added from a `trait_mod:<is>(Variable ...)` handler through
`$v.block.add_phaser` now runs in the declaring block's own queue: an `ENTER`
phaser runs before the block body, not at the declaration. `LEAVE`, `KEEP` and
`UNDO` are accepted too and run on block exit by outcome. The interim
per-declaration replay marker is gone; the declaration only keeps a value the
`ENTER` phaser seeded through `Variable.var`.
