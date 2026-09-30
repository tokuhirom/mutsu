# A `state` array initializer decays its `Nil` items like `my`

`state @s = Nil, Any` (statement or expression position) used to keep the
`Nil` item in the array, where `my @s = Nil, Any` and Rakudo store `Any`.
`StateVarInit` now runs the same Nil-item decay as the `@` store of `SetLocal`
(`decay_nil_elements_for_var_assign`, ADR-0049), so a `Nil` item becomes the
container's default: `Any`, the element type object for `state Int @a`, or the
`is default(...)` value.

Closes #10357.
