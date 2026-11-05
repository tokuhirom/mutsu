# No sink warning for the tail of a loop body with KEEP or UNDO

A loop body that has a `KEEP` or `UNDO` phaser no longer gets "Useless use of constant ... in sink context" for its last statement, as in rakudo, since the phaser reads the iteration's value. Bodies without the phasers still warn.
