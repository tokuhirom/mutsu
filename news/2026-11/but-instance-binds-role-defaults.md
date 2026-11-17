# `$obj but R` binds a parametric role's defaults

`$obj but R` on an instance is now a copy of `$obj` reblessed into the mixin
type `C+{R}`, the same in-place composition `$obj does R` performs (rakudo's
`but` is `.clone.does`), for every role rather than only roles with
attribute-trait hooks (#17765). A parametric role's parameter defaults are
therefore bound for its `TWEAK` and its methods: `C.new but R` with
`role R[Bool:D :$reset = True]` now sees `$reset` as `True` instead of an
undefined value.

Two smaller differences from rakudo went with it. An untyped role parameter
left unsupplied (`role R[:$x]`, `role P[$y?]`) now reads `Mu`, as role
parameters are `Mu` parameters, not `Any`. And a type renamed with
`.^set_name` -- a `but` copy's mixin type or a plain class -- renders its
instances' `.raku` by the new name.
