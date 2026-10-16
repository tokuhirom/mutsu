# An enum member's methods are rows of Enumeration

An enum member's `key`, `value`, `kv`, `pair`, `gist`, `raku`, `Str`, `pred`, `succ`, `ACCEPTS`, `Int`, `Numeric` and
`Real` are now rows of the roles Rakudo composes into every enum type: `Enumeration`, and `NumericEnumeration` or
`StringyEnumeration` for `Str` (ADR-11276 §9.125). A method those roles do not declare is answered by the member's base
type on its value, as in Rakudo, where an enum type inherits from its base: `y.is-prime` of an `Int`-valued enum and
`p.uc` of a `Str`-valued one now answer, and `.WHICH` stays the enum's own.
