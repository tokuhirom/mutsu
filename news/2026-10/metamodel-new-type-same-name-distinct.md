# `Metamodel::ClassHOW.new_type` with a repeated name mints a distinct type

Calling `Metamodel::ClassHOW.new_type(:name<Foo>)` twice used to hand back two meta-objects
that shared one registry entry, so the second type's methods and attributes overwrote the
first's. Red 0.2.7 does this on purpose (`Specialisable.!specialise` creates a type with the
same name as its model), and its relationship queries mixed the two types up.

A name that is already minted, or already composed, now gets a unique site-keyed suffix in the
registry (`Name\0<digits>`, the same shape ADR-0047 uses for lexical sites), while `.^name`
keeps reporting `Foo`. Role arguments of such a type are recognised as type names rather than
re-parsed as source text.

Refs #14094, #14711.
