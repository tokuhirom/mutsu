# A package block inside a routine knows its own name

`sub f { module A { say $?PACKAGE.^name } }; f()` printed `GLOBAL`: inside a
routine the compiler folds `$?PACKAGE` to the routine's declaring package, and
a nested package block did not override it. A module declared in a routine of
`module Q` (directly or in code the routine EVALs) was also left unqualified
(`R` instead of `Q::R`), because the routine's compile-time package is its
mangled state scope (`Q::&f/0`), which package qualification skipped.

A package block now sets its own `$?PACKAGE` for its body, and a package
declared inside a routine is qualified by the package the routine was
declared in, as on rakudo (#18480).
