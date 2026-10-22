# A variable trait in a role body runs once, on a Sub

`role R { my $rv is foo; ... }` ran the `trait_mod:<is>(Variable ...)` handler once per
composition, with `Variable.block` a `Block`. Rakudo compiles the role body once, so the
handler runs once and `block` is a `Sub`. The BEGIN-prologue lift now walks a role body's
variable declarations that carry a user trait, gives the role frame the `Sub` owner and
skips the scope phaser replays a role body has no scope for (#14532).

A role-body variable with an initializer is still applied per composition: the lift
splits the declaration from its assignment, and a role's methods capture the variable's
value at the declaration (#14616).
