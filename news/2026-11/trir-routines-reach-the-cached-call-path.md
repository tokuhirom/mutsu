# A TRIR routine's calls reach the cached dispatch

A routine whose body compiles to TRIR -- the typed fast tier, which a body as
small as `sub sp($ch) { 1 }` qualifies for -- ran its chunk on its first call
straight from the general dispatch and returned before the name-keyed
positional-light cache entry was written. Nothing else ever wrote it, so every
later call to the routine repeated the full name resolution of the general
path before running the same chunk again: about 13,000 instructions per call,
more than the same helper written with a typed signature that TRIR does not
take (7,900).

The general dispatch now decides whether the positional-light path serves the
call, and fills the cache, before it tries TRIR. The second and every later
call takes the cached dispatch, which tries TRIR first itself. `sp($c)` in a
loop went from 13,000 to 5,500 instructions per iteration (release build),
closing #17441.
