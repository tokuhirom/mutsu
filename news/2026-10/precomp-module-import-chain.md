# A module's compiled code no longer depends on the importer's lexical imports

A module's mainline was compiled with the lexical import chain of the code
that happened to load it first, so its `where` chunks (for example in
`Test::is-approx-calculate`) differed between importers while the precompiled
cache is keyed by module alone. The nightly `precomp-verify` stress job caught
it as exit status 70 on four roast files. A module's lexical scope now starts
empty, and the verify diagnostic strips symbol ids so the first reported
difference is the real one.
