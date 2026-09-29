# A `for` loop over a `.map`/`.grep` Seq pulls one iteration at a time

A `for` loop over a not-yet-run `.map` or `.grep` Seq (ADR-0058's
`SeqSource::MapGrep`) used to reify the whole Seq before its first iteration,
so the callback ran over every source element before the loop body ran once.
That was observable, not only slow: `for (1..3).map({ print "m$_ " }) { print
"b$_ " }` printed `m1 m2 m3 b1 b2 b3` where Rakudo prints `m1 b1 m2 b2 m3 b3`,
and `for @big.map(*+1) { last }` ran the callback two million times (17s in a
debug build) where Rakudo runs it once (#9936).

The loop now claims the Seq's source and pulls one iteration's worth of
elements per iteration through the existing prefix pull
(`pull_map_grep_prefix`, #9158), so the callback and the body interleave and a
`last` leaves the rest of the source unmapped. Multi-parameter loops pull
`arity` elements per iteration, a typed parameter still autothreads a Junction
element, a callback that dies ends the loop between iterations, and whatever
the loop pulled is stored back in the Seq afterwards, so the Seq stays readable
as before. Callbacks the prefix pull cannot run a chunk at a time (a
multi-parameter block, a `FIRST`/`LAST` phaser, a shaped source) are still
pulled whole on the first iteration.

A rw map (`for @a.map({ $_ *= 10 }) { last if ... }`) now also writes back only
the elements it reached, as in Rakudo. Its per-chunk writeback used to rebuild
the whole source container, which made the one-element pulls quadratic; a later
chunk now writes just its own slots.

Follow-ups: `.iterator`/`pull-one` over a deferred map still reifies the whole
source (#10186), and a one-element pull pays the whole map-loop setup, so a
streamed loop that runs to the end costs about 3x the bulk map per element
(#10187).
