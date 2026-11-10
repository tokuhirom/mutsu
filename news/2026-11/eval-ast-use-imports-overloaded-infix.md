# EVAL($ast): an infix a unit's own `use` imports is an overloaded operator

`use ExportedInfixFamilyBase; $n == $dt` parsed from source calls the imported `infix:<==>`
candidates with the variable's container, so the `is rw` candidate wins over the
`is readonly` one. As an `EVAL($ast)` the lowering did not know the `use` had imported an
operator (the parse that read the `use` is a different one), kept a plain `==` and passed
the value, so the read-only candidate won.

Lowering a `use` now records its exports the way the parse does (and a `use lib` adds its
path first), scoped to the unit being lowered, and the infix lowering asks for them beside
the operators the unit declares. `exported-infix-family-ranks-candidates.t` joins the
`EVAL($ast)` ratchet.
