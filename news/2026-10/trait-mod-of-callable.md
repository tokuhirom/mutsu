# `trait_mod:<of>` and `trait_mod:<returns>` are callable

A distribution's own routine-trait handler can now re-dispatch to CORE's `trait_mod:<of>` /
`trait_mod:<returns>` (`trait_mod:<of>($method, Path::Finder:D)`), as it can for `trait_mod:<is>`
and `trait_mod:<does>`. The candidates are a prelude over a native primitive that records the
return type on the routine; a routine that already declares one throws a redeclaration error.

Found through Path::Finder, whose `use` died with `Unknown function: trait_mod:<of>`. It now loads
and 9 of its 14 test files pass under mutsu like rakudo; the rest are tracked in #13148, #13149 and
#13150.
