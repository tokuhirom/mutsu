# `Nil` through a name bound to a typed attribute resets it to its default

`my $r := $obj.acc; $r = Nil`, where `acc` is an `is rw` method (or a public
`is rw` accessor) over a typed attribute such as `has Int $!x`, died with
"expected Int but got Any". The untyped name `$r` decayed the `Nil` to its own
`Any` before the store reached the attribute's cell, and the cell's `Int`
constraint then refused it. Two changes fix it:

- A `Nil` stored through a name that aliases a typed cell now resets to that
  cell's type object.
- An attribute's promoted cell carries the attribute's `is default(...)`, so
  the store yields that value instead when one is declared.

Both match Rakudo, where the container's descriptor decides (#18755).
