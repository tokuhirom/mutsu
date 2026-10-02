# A bind through `OUR::` tracks its source

`$OUR::x := $y` rebound only an entry named `OUR::x`, while an `OUR::` read
(and a `$Pkg::x` read inside `package Pkg`) looks the variable up under its
resolved key -- the bare name at file scope, `Pkg::x` in a package -- and
found the value the bind had snapshotted. A later `$y = 45` was therefore
invisible through `$OUR::x` although `=:=` said the two were one container.
The bind now also rebinds the resolved `our`-store entry (and the qualified
env key inside a package); a lexical `$x` that aliased the old `our`
container keeps it, as in Rakudo (#10859).
