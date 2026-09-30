# An undeclared word between two terms is "Two terms in a row"

mutsu used to accept any non-reserved word as an infix operator and resolve it
at run time against a routine of the same bare name. So `(1,2) cross (3,4)`,
`(1,2) zip (3,4)`, `(1,2) join (3,4)` and even `sub foo($a, $b) {...}; 1 foo 2`
all ran, and an unknown `1 foo 2` failed only once execution reached it, after
the statements before it had already run. Rakudo rejects every one of these at
compile time with "Two terms in a row".

The word-infix matcher now takes a word only when an `infix:<word>` is declared
in scope or rakudo's `CORE::` declares `&infix:<word>` (the vendored table from
ADR-0093). A statement that stops before a separator on the same line is now a
compile-time "Two terms in a row", so the leftover `cross (3,4)` can no longer
be silently read as a second statement. The export-hook scan also picks up
operators that a `sub EXPORT` binds as `my &infix:<op> = ...`, so an importer's
parse still knows about them. This closes the divergence ADR-0093 left open
(#9918).
