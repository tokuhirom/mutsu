# `my $x will leave { }` at the top of a program runs when the program ends

`my $x will leave { ... }` (and `will keep` / `will undo`) written in the main
program's own scope never ran: the `will` expansion wraps the declaration and
its exit phasers in a scopeless `SyntheticBlock`, and the unit's phaser split
(`split_block_phasers`) only looks at top-level statements, so the phasers were
compiled as no-ops. Inside a block or a sub they already worked. The phasers are
now lifted out of that block, to the front of the unit's list, so they run after
the program's `LEAVE { }` blocks, each group last declared first, as rakudo
2026.09 orders them (measured on `my`/`our`/`state` variables mixed with several
`LEAVE` blocks). Found by the raku-doc differential (`Language/phasers.rakudoc`).
The block-level ordering of traits against `LEAVE` blocks is #16606.
