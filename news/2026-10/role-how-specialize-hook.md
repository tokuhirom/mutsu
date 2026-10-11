# A user `specialize` on a role's HOW now runs when a class composes the role

A `trait_mod:<is>` on a role method now runs with `$*PACKAGE` bound to the individual parametric
role, so `$*PACKAGE.HOW` is a `ParametricRoleHOW` (it was the role's `ClassHOW`). A role mixed into
that HOW with a `specialize` method is driven when a class composes the role, after the class is
registered, as `Method::Also` relies on. `Role.^multi_methods_to_incorporate` exists too (always
empty: a role's multi candidates are already part of its definition).

Not covered: a `my $r` scalar still shares its slot with a sigilless `\r` in the same scope, which
keeps the unmodified `Method::Also` role path (t/01-basic tests 10-11) from passing.
