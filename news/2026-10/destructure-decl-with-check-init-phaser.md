# `my ($a, $b) = RHS` in a unit with a CHECK or INIT phaser

A unit that has a `CHECK` or `INIT` phaser reorders its top-level statements and
splits each bare declaration into a hoisted static half and an assignment.
`split_var_decl` decided "no initializer" from `has_initializer` alone, but the
expansion of `my ($a, $b) = RHS` builds its staging temp and its targets with
`has_initializer: false` and a real initializer expression, so the split dropped
the right-hand side and every target stayed `Any`. A declaration whose
initializer expression is not the default literal of a bare declaration is now
kept whole (#15728).
