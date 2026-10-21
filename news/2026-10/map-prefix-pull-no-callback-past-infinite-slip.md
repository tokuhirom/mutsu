# A prefix pull of a `.map` no longer runs a callback past an infinite Slip

`(1, 2, 3).map({ $n++; slip(1..*) }).head(2)` now runs the callback once, as Rakudo does. A
prefix pull used to cut its chunk at `missing` source groups, so the second callback ran
before the first one's infinite Slip was seen. The pull now starts one group at a time and
widens to `missing` groups only after a chunk answered exactly one element per group, which
keeps the plain one-to-one `.map(...).head(n)` in a single chunk.
