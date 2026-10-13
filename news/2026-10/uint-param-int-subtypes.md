# A `UInt` parameter accepts every Int-shaped type object and Int-subclass instance

`UInt` is `subset UInt of Int where { not .DEFINITE or $_ >= 0 }`, so the `Bool`, `IntStr` and
user `Int`-subclass type objects, a non-negative instance of an `Int` subclass, and the type object
of an `Int`-based enum all satisfy it. mutsu rejected all of them. The `UInt` arm of
`type_matches_value` now defers to the `Int` check for those shapes, and the `UInt` check accepts an `Int`-based enum type object
(`E ~~ Int` is still False; see the TODO there). Closes #12932.
