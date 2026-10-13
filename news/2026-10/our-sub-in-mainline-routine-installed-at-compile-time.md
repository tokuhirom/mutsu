# An `our sub` in a mainline routine is installed in GLOBAL at compile time

`sub f { our sub helper { 1 } }; say GLOBAL::helper()` printed `Could not find
symbol 'helper' in 'GLOBAL'` unless `f` had run; rakudo installs the routine
when the unit is compiled. The CHECK-time prepass that already did this for an
`our sub` nested in a package (#10504) now also collects one nested in a routine
or block outside every package, and installs it in the unit's package (`GLOBAL`,
or an EVAL's caller's package). As before, it closes over the declaring
activation's lexicals, which are undefined until that activation runs (#13091).
A bare `helper()` at the mainline, which rakudo rejects at compile time, is
#13111.
