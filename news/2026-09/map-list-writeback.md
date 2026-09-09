# `.list.map` preserves writable Array elements

`map` now keeps the backing Array identity when its receiver is a real Array
reached through a value-producing expression such as `@a.list` or a scalar
holding `[1, 2, 3]`. A callback that assigns to `$_` therefore writes through to
the source Array when the deferred `Seq` is consumed, matching Rakudo.

Immutable `List` receivers remain on the readonly topic path.
