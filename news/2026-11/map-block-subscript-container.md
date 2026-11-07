# A `.map` block ending in a subscript hands back the element's container

`my ($v is rw) := (1,).map(-> $k { %h<a> })` died with "Cannot assign to an immutable value":
the block's last-statement subscript was read as a value, while rakudo returns the element's
`Scalar`, so a Seq built by `.map` holds containers that an `is rw` signature, `:=` or a
`for ... -> $e is rw` loop binds and writes through (CustomImporting's `from-import`).
The `.map`/`.grep` block recompile now compiles a tail subscript (or the branches of a tail
`if`/ternary) as an element container, the way an `is rw` routine's tail is; other tails stay
plain values.
