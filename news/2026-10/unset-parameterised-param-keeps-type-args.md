# An unset parameterised parameter keeps its type arguments

An omitted optional or named parameter constrained to `Positional[Int]`,
`Array[Int]` or `CArray[int32]` used to bind the bare type object (`Positional`),
so `$!a := $a` in `BUILD` failed the attribute's type check. The unset parameter
now binds the parameterised type object, as `my Positional[Int] $x` does
(#12105).
