# `$obj but R` binds a parametric role's defaults

`$obj but R` on an instance, for a parametric `R`, is now a copy of `$obj`
reblessed into the mixin type `C+{R}`, the same in-place composition
`$obj does R` performs (rakudo's `but` is `.clone.does`) (#17765). The role's
parameter defaults are therefore bound for its `TWEAK` and its methods:
`C.new but R` with `role R[Bool:D :$reset = True]` now sees `$reset` as
`True` instead of an undefined value. Other roles keep the mixin wrapper,
which still carries the MRO, `WALK` and per-role attribute storage the
reblessed type lacks.

Two smaller differences from rakudo went with it. An untyped role parameter
left unsupplied (`role R[:$x]`, `role P[$y?]`) now reads `Mu`, as role
parameters are `Mu` parameters, not `Any`. And a class renamed with
`.^set_name` renders its instances' `.raku` by the new name.
