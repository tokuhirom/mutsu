# A dynamic variable falls back to the same-named GLOBAL package variable

Rakudo's dynamic lookup of `$*X`, `@*X` and `%*X` ends at `GLOBAL`: with no
`my $*X` in the dynamic scope, `%GLOBAL::X` answers `%*X`. mutsu stopped at the
dynamic scope and `PROCESS`, so a read died with "Dynamic variable %*X not
found" or came back undefined. Red keeps its default drivers that way
(`%GLOBAL::RED-DEFAULT-DRIVERS = ...` read as `%*RED-DEFAULT-DRIVERS`), which is
why `red-defaults` and `get-RED-DB` reported "$*RED-DB wasn't defined" (#14094).
The scalar, array and hash reads now try `GLOBAL::` last, after every
declaration in the dynamic scope.
