# A `UInt` parameter accepts every Int-shaped type object and Int-subclass instance

`UInt` is `subset UInt of Int where { not .DEFINITE or $_ >= 0 }`, so the `Bool`, `IntStr` and
user `Int`-subclass type objects, a non-negative instance of an `Int` subclass, and the type object
of an `Int`-based enum all satisfy it. mutsu rejected all of them. The `UInt` arm of
`type_matches_value` now defers to the `Int` check for those shapes, and an `Int`-based enum type
object is an `Int` type (`E ~~ Int`, `sub f(Int $x)` called with `E`). Closes #12932.
