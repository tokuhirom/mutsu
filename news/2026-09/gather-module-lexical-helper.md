# Preserve module scope while compiling a gather body

Lazy `gather` bodies now retain the package of the routine that created them.
Bare calls from an exported module routine can therefore reach that module's
lexical helper instead of being resolved in `GLOBAL` when the gather is forced.
