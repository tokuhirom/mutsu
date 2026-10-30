# Two types minted under one name stay two types

`Metamodel::ClassHOW.new_type(:name<Foo>)` called twice already gave the second type its own storage
identity (`Foo\0<id>`), so methods, HOW and `===` were separate. The type check still matched an
instance of the second type against the first through the demangled name, so `B.new ~~ A`,
`.does` and `.ACCEPTS` answered True. A type minted under another type's name is no longer matched
through its demangled name (#14711).
