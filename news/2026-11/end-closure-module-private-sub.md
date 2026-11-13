# A module's END closure can call the module's own `my sub`

A closure built inside a module's `END` block (a typed pointy such as
`if $n -> uint $t { ... }`, a `-> $t { ... }`, an anonymous sub) died with
"Unknown function" when it called the module's own `my sub`. `END` bodies run
at program exit with `?FILE` still naming the main script, and a closure
records `?FILE` as its compunit, so it lost the module's private routines.
The END runner now scopes `?FILE` to the declaring module while the body runs,
as the module's mainline load does (#17577).
