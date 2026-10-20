# An enum type's `.^mro` lists its base type

`enum E <alpha beta>` inherits from `Int` in Rakudo, so `E.^mro` is
`(E Int Cool Any Mu)` and `E.^parents` is `(Int)`; mutsu answered
`(E Any Mu)` and `()`. The MRO of a user enum now follows its base type's
catalog chain, the base being its first value's type (`Int`, `Str`, `Rat`,
`Array`, ...) (#13667).
