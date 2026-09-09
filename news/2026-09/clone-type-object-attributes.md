`Mu.clone` now rejects attribute overrides on type objects with Raku's
`Cannot set attribute values when cloning a type object` error. Argumentless
type-object clones and attribute overrides on defined instances keep their
existing behavior.
