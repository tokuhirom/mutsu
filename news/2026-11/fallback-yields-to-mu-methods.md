# A class's FALLBACK no longer catches the methods every object has

On a class with a `FALLBACK`, `T.isa(U)`, `.Str`, `.DUMP` and a class-level
attribute's accessor reached the `FALLBACK` instead of `Mu`'s own method,
because the wildcard-delegation/`FALLBACK` step of method dispatch ran before
`Mu`'s late rows. Rakudo's `FALLBACK` only sees a name normal dispatch did not
find, so that step now runs after them. EEL's runner asks `$type.^isa(Event)`
of every pattern type, and `Event` has a `FALLBACK`: 37 of EEL 0.3's 43 test
files died there (#16888).
