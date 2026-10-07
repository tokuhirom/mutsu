# A nested `CArray[CArray[T]]` variable type is the real parameterised type object

`my CArray[CArray[uint8]] $v` used to seed its type object from the spelling of the inner
parameterisation, so `$v.^name` was unqualified, `.REPR` was `P6opaque` and `$v ===
CArray[CArray[uint8]]` was false. A nested type argument is now evaluated through its own
`^parameterize`. `.of` on such a type returns the type argument itself (keeping the inner
parameterisation), and `.can('of')` / `.^can('of')` find the dispatch-provided `.of` of a
parameterised mixin. Closes #12203.
