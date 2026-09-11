# Builtin MOP queries preserve concrete and parametrized ancestry

`.^isa` now follows the builtin type catalog for concrete values, so queries
such as `"x".^isa(Cool)`, `True.^isa(Int)`, and `1.5.Rat.^isa(Cool)` agree with
Rakudo. Previously every non-package receiver returned `0` from this MOP
entry point even though ordinary `.isa` and method dispatch already knew the
correct ancestry.

`.^mro` also now expands parametrized builtin type names through their base
catalog row. `Array[Int].^mro` therefore includes `Array`, `List`, `Cool`,
`Any`, and `Mu` after the parametrized head instead of stopping at
`Array[Int]`.

The regression coverage is in `t/oo/class/builtin-catalog-mro.t`.

Closes #7937.
