# Package stashes serve registered routine values

Reading an `our sub` or proto from its package stash now resolves the registered
routine instead of returning a frame-captured code value. The result reports
`Sub`, keeps the same identity as a qualified routine reference, and a proto
read this way dispatches to its multi candidates. Qualified routine values
also report their unqualified `.name`.
