# A `.map` / `.grep` block runs its `FIRST` and `LAST` phasers

A `.map` or `.grep` block iterates like a loop body, so Rakudo runs its
`FIRST` phaser once, before the first call. A map block's `LAST` phaser runs
once, after the last call. A grep block runs no `LAST`.

mutsu used to differ in two ways:

- It never ran `FIRST`.
- It ran no phaser at all for a `.map` over an array, because that map
  rw-binds each element.

Every map and grep loop now runs these phasers through one helper:

- the inline loops;
- the full-binding loops for typed or multi-parameter blocks;
- the rw map;
- the lazy pipe over an infinite source (#14299).
