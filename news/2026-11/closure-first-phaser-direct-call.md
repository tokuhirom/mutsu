# A block's `FIRST` fires on a direct call

`my $c = { FIRST $g++; 1 }; $c() for ^3` now runs the `FIRST` phaser
once. A sub's `FIRST` also runs on its first call. Before, the only
place a block's `FIRST` ran was the `for` loop or the `.map`/`.grep`
call that iterated it, so a direct call never ran it.

`FIRST` belongs to the code object, as in rakudo: the first call of each
closure clone runs it at entry, before `ENTER`, whoever makes the call. A
callable body now starts with a `FirstPhaser` opcode that takes the
running clone's flag, keyed by the clone's state scope. A `.map` loop
sets that scope to the callback's clone too, so one block value fires
once across `.map`, `.grep` and direct calls. A `.map` block's `FIRST`
now also sees the first element as its topic. The loops' separate
`FIRST` runs are gone (#17945).
