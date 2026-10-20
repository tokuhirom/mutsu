# A prefix read of a multi-parameter `.map` runs only the groups it needs

`my $u = (1..10).map(-> $x, $y { ... }); $u[1]` ran the callback over all five
groups before answering, where Rakudo runs it twice. A deferred `.map` can be
pulled a chunk of its source at a time, but a block with more than one
positional parameter was excluded from that, so any read of it drained the
whole source first. The same happened after a chained `.map`/`.grep`.

The prefix pull now cuts its chunks at whole groups: one call of `k`
source elements for each element still missing, and a chained map waits until
the upstream has produced a whole group (or run dry) before calling the block.
A multi-parameter `.grep`, a slurpy parameter and a `FIRST`/`LAST` phaser still
read their source whole (#14196).
