# `Nil` and type-object parameter defaults no longer leave the light call path

A routine whose optional parameter defaults to a constant is called through the light path, which
fills the omitted parameter from a table built at registration. The table admitted only Int, Num,
Str, Bool and Rat literals, so `$x = Nil`, `$x = Any` or `Int $x = Int` sent the whole call through
the general binder, which saves and restores the topic and the parameter's env binding around the
default's evaluation (#14812).

`Nil` and a type object are singletons with nothing to share or mutate, so they now count as
immutable literals: the light path fills them and the general binder returns them without
evaluating the default. The declared constraint is still checked once at registration, so
`Int $x = "no"` still stays on the general binder.

callgrind (profiling build, warm, 300 minus 100 calls of `sub f($a, $p = Nil) { 1 }`): 27,924 ->
5,093 instructions per call; `$p = Any`: 28,524 -> 4,124 (a literal Int default is 4,105).
AttrX::Mooish's `invoke-opt` (`Capture:D $params = \()`, `PvtMode :$private is copy = pvmAuto`) is
not covered: a capture literal and an enum constant still take the general binder.
