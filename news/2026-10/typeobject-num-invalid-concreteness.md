# `.Num` on a concrete-only type object names the type object

`Int.Num`, `Str.Num`, `UInt.Num`, `Complex.Num`, `Rat.Num` and `FatRat.Num` used to die with
"No such method 'Num'". They now raise `X::Parameter::InvalidConcreteness` ("Invocant of method
'Num' must be an object instance of type ...") exactly as Rakudo does, matching the existing
`Num.Int` guard. `Num.Num` stays the identity. Closes #11992.
