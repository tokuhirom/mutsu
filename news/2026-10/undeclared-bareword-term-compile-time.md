# An undeclared bareword term is a compile-time error

`say FooBarBaz.^name; say "alive"` used to print `Str` and `alive`: an
undeclared bareword term fell through to the run-time bareword lookup, whose
last resort is the name itself as a `Str`, so a typo'd type name silently
became a string. Rakudo rejects the program at compile time, before anything
runs.

The mainline now runs the same undeclared-name walk `EVAL` already used
(`src/runtime/undeclared_names.rs`), right after the CHECK-time
undeclared-routine check, and reports `X::Undeclared::Symbols` ("Undeclared
name") with the term's line. Declarations are collected scope-blind from the
whole unit, and a unit that imports names the walk cannot see (any `use` other
than a pragma or `Test`, `need`, `import`, `require`) is not judged, so the
check errs only in the safe direction. Pair keys, colonpairs and angle words
are untouched (#9768).
