# min/max skip any undefined operand, not just Any

`min(2, Int)`, `2 min Int` and `(2, Int).min` disagreed: the method form already
skipped an undefined operand and returned `2`, but the `min`/`max` subs and the
infix operator only special-cased the bare `Any` type object, so `min(2, Int)`
returned `(Int)` instead of `2`. `max` happened to return the right answer by
accident, since the undefined operand's fallback string comparison sorted it
after the defined one for `max` but before it for `min`.

The two remaining hand-rolled folds -- the interpreter's `min`/`max` sub form
(`extrema_from_values_generic`, also used by `.min`/`.max` with 3+ candidates
or a `:by` block) and the VM's `minmax_two` two-argument fast path and
`min_max_values` infix operator body -- now all skip an operand using the same
`value_is_defined` check the array `.min`/`.max` path already used, matching
Rakudo and removing the drift between the four implementations.
