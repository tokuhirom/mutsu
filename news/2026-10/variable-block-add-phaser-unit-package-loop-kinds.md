# Variable.block.add_phaser works at unit level and in class bodies, with every phaser kind

A phaser a `trait_mod:<is>(Variable ...)` handler adds through
`$v.block.add_phaser` now reaches the declaring block's queues for unit-level
declarations and class or package bodies too, not only nested blocks. The unit
and the body run `ENTER` before their statements and `LEAVE`/`KEEP`/`UNDO` on
exit. `FIRST`, `NEXT`, `LAST`, `PRE` and `POST` are accepted as well.

Along the way a class body's own `ENTER`/`LEAVE` phasers stopped running at
declaration time when the BEGIN prologue declared the class ahead: they run
around the body's statements, as in Rakudo.
