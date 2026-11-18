# `$*CU` read in a CHECK resolves the types the unit imported

A type the unit imported (`use static :DEFAULT, :types` brings in
`CheckFailure`) is no class-table key under its short name: the class is
`static::CheckFailure`. The resolver that builds `$*CU` when a CHECK phaser
first reads it asked only the class table whether a short name is a type, so
`* ~~ CheckFailure` in the CHECK block stayed unresolved and static's analyzer
died with "'CheckFailure' has not been resolved". It now also accepts a type
the scope binds by that name, as the rebuild at the end of the BEGIN prologue
already did. static's `t/07-diagnostics.rakutest` passes again.
