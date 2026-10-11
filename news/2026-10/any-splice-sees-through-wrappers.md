# `Any.splice` sees through cells, variable references and mixins

The `Any.splice` row declined every `ContainerRef`, `VarRef` and `Mixin` receiver. It now
unwraps the receiver once and declines only when another entry owns the real `splice`
(an `Array`, a `Buf`, an `is Array` instance, a user `splice`). `(5 but Tag).splice(1)`
names its type `Int+{Tag}` in the `X::Multi::NoMatch` message, as Rakudo does (#12653,
ADR-11276 §9.69).
