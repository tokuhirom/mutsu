# A streamed `.grep` sets up its loop once per pull, not once per element

A deferred `.grep` that is consumed a prefix at a time — `for @a.grep(...)`,
the upstream of a chained `@a.grep(...).map(...)`, `.head`, an `Iterator` —
used to run its inline loop over one source element per loop run, because a
pull of `n` elements could only ask for a chunk of `n` *source* elements. A
grep that skips elements therefore paid the whole loop setup (the env merge of
the callback's captures, the register reset of a nested VM run, the result
array) for every element it looked at, which cost more than the callback.

The grep loop now reads its source lazily through a `GrepFeed` and stops at
the `n`-th match, so one loop run serves the whole pull however many elements
it skips. The source is read as the loop reaches each element, so a Live
array's pushes during the stream are still seen, and the callbacks still
interleave with the consumer exactly as before.

Measured on a 200 000-element source (release build, median of 7 runs, same
box): `@a.grep({ $_ %% 3 }).map({ $_ * 2 }).elems` 477 ms → 311 ms, and
`for @a.grep({ $_ %% 3 }) { $n++ }` 399 ms → 280 ms. Under callgrind at 20 000
elements the chain went from 254M to 190M instructions (#11515).
