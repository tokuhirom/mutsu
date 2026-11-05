# An EVAL'd `use lib` no longer hides a later `sub EXPORT` hook's names from the CHECK-time check

The pre-run that loads an EVAL'd unit's top-level `use` statements ahead of its CHECK-time name
check stopped at the first `use lib`, so `EVAL q[use lib 't/lib'; use Hook; Thing]` rejected the
term the hook exports. A top-level `use lib` with literal arguments is now pre-run itself (it only
adds the search path, which the unit's own run finds already in front), and the loads after it
follow. A `use lib` with a computed argument, a nested one, or a `BEGIN` block still ends the
pre-run. Part of #13441.
