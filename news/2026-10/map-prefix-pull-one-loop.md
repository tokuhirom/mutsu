# A deferred map is read by prefix in one loop run

A prefix read of a deferred `.map` (`head(n)`, `[i]`, `first`, boolification) now runs the
map loop once with a produced-element limit over a fetch feed, as `.grep` already did. The
loop reads each source element when it reaches it and stops itself after the callback has
produced what the reader asked for, so it no longer copies the rest of the source per pull
and the "start with one group, widen once a chunk answered one element per group" batching
of the chunked pull is gone for `map` (#14196).

The rw map (`@a.map({ $_++ })`) follows: its prefix pull is also one fed loop run, only
the elements it consumed are written back, and the chunk-size batching of
`pull_map_grep_prefix_into` is deleted.

A chained `.map(...).map(...)` pull reads through the same feed (capped at the upstream
elements already pulled), and a short final group of a multi-parameter map now raises
raku's `Too few positionals passed; expected 2 arguments but got 1`.
