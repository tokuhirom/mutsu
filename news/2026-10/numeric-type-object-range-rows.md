# `.Range` of the numeric type objects comes from the method table

`Int.Range`, `Num.Range`, `Rat.Range`, `FatRat.Range` and `Rational.Range` are rows of the method table now (ADR-11276 §9.97),
and a native integer type object such as `int8` or `uint16` reaches `Int`'s row through its MRO, so `int8.Range` is
`-128..127` as before. Two answers moved to Rakudo's. `Real.Range` throws `X::Method::NotFound`, since `Real` declares no
`Range`. And `.Range` of an instance (`5.Range`) throws `X::Parameter::InvalidConcreteness`, because the method belongs to the
type object.
