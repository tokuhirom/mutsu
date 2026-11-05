# Shaped-array writes that do not fit die with rakudo's messages

`my @d[2]; @d = 1, 2, 3` now dies with `Index 2 for dimension 1 out of range
(must be 0..1)` instead of silently dropping the extra items, as a write past a
fixed dimension (`@d[2;0] = 1`, `@d[0;2] = 1`) names the dimension and its range
instead of saying "Index out of bounds". `X::NotEnoughDimensions` and
`X::Assignment::ToShaped` carry rakudo's message text (`Cannot assign to a 2
dimension array with only 1 dimensions`, `Assignment to array with shape 2 2 must
provide structured data`).
