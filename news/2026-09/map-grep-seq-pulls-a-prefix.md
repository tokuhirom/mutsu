# A `.map` / `.grep` Seq runs its callback only over what the consumer needs

A deferred `.map` or `.grep` Seq (ADR-0058) used to run its callback over the whole source
the first time anything touched it, whatever that consumer needed. So `@a.map(&f).head(3)`
called `f` once per element of `@a`, and `?@a.grep(...)` ran the predicate over every element
just to learn whether there was at least one match.

Now a consuming `.head(n)` / `.first` and a boolification pull a prefix, the way Rakudo's
pull-one iterator does (`src/vm/vm_map_grep_pull.rs`). They drive the ordinary map/grep loops
over successive chunks of the source. Each chunk is as long as the number of elements still
missing, and every element produced needs at least one source element, so the callback never
runs over an element the consumer did not need. Boolification keeps the Seq: it stores the
prefix in the body and resumes from the source position `SeqSource::MapGrep::pos` on the next
read. A `last` in the callback ends the Seq whichever pull hits it. The loops report it
through `map_grep_last_depth`, which is stamped with the loop-handler depth so that a `last` a
nested loop caught does not count.

A non-shaped Array source is no longer copied at the `.map` call (`MapGrepItems::Live`). It
is read at pull time, as Rakudo iterates the Array, so `my $m = @a.map(&f); @a.push(4)` maps
the pushed element too. The rw map and the promoting grep write back or promote only the
chunk they ran over.

A callback that binds several elements per call, or that has a `FIRST` / `LAST` phaser,
still pulls its whole source, because the loops fire those phasers once per run.

Measured with `scripts/array-complexity-check.sh` on a release build in the same session,
`map(...).head(3)` (10 calls, 200 000 elements) went from 1.25 s to 0.0007 s. Its N-doubling
ratio went from 2.20 to 0.87. For `?@a.grep` (200 calls, 10 000 elements), #9158 recorded
2.49 s with a ratio of 1.95. `scripts/vm-complexity-check.sh` now reports 0.003 s with a
ratio of 1.03. This is part of #9158.
