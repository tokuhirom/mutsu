# The bytecode cache records the static-outer latch as a compile input

The static-outer latch (#18787) is set by the first unit a process reads that
mentions a dynamic neighbour's lexical chain, such as `$CALLER::OUTER::x`;
`Test.rakumod` is one. Under it, a sub declared below its unit's top level
carries a static view of its declaring scope. The compiler read the latch
directly, so a module compiled and cached by a process that had not set it was
served unchanged to one that had, and the other way round. The nightly
`precomp-verify` job caught it: 74 `t/` files exited 70 once the order in which
test files filled the shared cache put an unlatched compile in front of a
latched load (#19166).

The compiler now asks for the latch through `compile_inputs`, so the answer is
recorded with the entry and a load in a process whose latch differs compiles
the module afresh. The cache format version moves to 74, which drops entries
written without the question.
