# A deferred map is read by prefix in one loop run

A prefix read of a deferred `.map` (`head(n)`, `[i]`, `first`, boolification) now runs the
map loop once with a produced-element limit over a fetch feed, as `.grep` already did. The
loop reads each source element when it reaches it and stops itself after the callback has
produced what the reader asked for, so it no longer copies the rest of the source per pull
and the "start with one group, widen once a chunk answered one element per group" batching
of the chunked pull is gone for `map` (#14196). The `rw` map keeps the chunked pull for now.
