# A class's `my constant` passed as a call argument stays the constant

Inside `class Q` with a `my constant Atom = 5`, a call like `k(Atom)` or
`say(Atom)` passed the type object `Q::Atom` once a nested class `Q::Atom`
had been declared. Rakudo passes `5` (#11385). The ecosystem module
Syndicate hit this with its exported `constant Atom`.

The bareword itself read `5`. Every plain call argument is also tagged with
its source name so an `is rw` parameter can bind the caller's container. For
`Atom` that tag was the bare spelling, and the tag's container lookup found
the package store's `Atom`, which is the nested type, and passed that in
place of the value. A bareword the compiler does not know as a variable
compiles to a plain term lookup and has no container. It now gets no source
tag.
