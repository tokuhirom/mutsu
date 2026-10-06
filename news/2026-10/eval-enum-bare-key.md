# EVAL resolves a bare enum key declared in the calling scope

`enum E <aa bb>; say EVAL "aa"` died with `Undeclared name: aa`; it now prints `aa`
like Rakudo, in the declaring scope, from inside a sub, and for a key a package-less
module file declares in `GLOBAL` (`use M; EVAL 'SA'`). A `unit module`'s private enum
key stays undeclared in the importer's EVAL, as in Rakudo.

Enum keys live in their own bare-name namespace (`runtime::enum_bare_names`), which the
EVAL undeclared-name and undeclared-routine checks never consulted; both now ask
`Interpreter::enum_bare_value` (#11818).
