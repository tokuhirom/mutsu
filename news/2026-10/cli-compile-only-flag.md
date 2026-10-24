# `mutsu -c`: compile only

`mutsu -c FILE` / `-c -e CODE` compiles the program, runs its BEGIN prologue and CHECK phasers
(last-declared first), prints `Syntax OK` and exits 0 without running INIT, the mainline or END
phasers, as `raku -c` does. A compile error prints the usual report and exits 1. Tools that use
`raku -c` as a CHECK-phase gate (the `static` distribution's whole test suite) can now start their
child (mutsu#14929). `static` then stops at its `$*RAKU.compiler.version >= v2026.09` gate, which is
the compat-version question recorded on the issue.
