# `.Complex` refuses receivers Rakudo has no candidate for

`Cool` declares `.Complex` (`self.Numeric.Complex`). mutsu's 0-arg coercion arm answered `0+0i`
for every receiver it could not read as a number. It now matches Rakudo: a non-`Cool` receiver
(`Blob`, a `Block`, a `Promise`, a `Pair`, a `Set`) is `X::Method::NotFound`, `Int`/`Rat`/`Bool`/
`Real` type objects are `X::Parameter::InvalidConcreteness`, `Num.Complex` cannot unbox the type
object, and a lazy list is `X::Cannot::Lazy`. `Str`/`List` type objects keep answering `0+0i`.
Closes #13116.
