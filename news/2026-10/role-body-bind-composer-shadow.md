# A role-body bind ignores the composer's same-named variable

The #11087 fix boxes a role body's `:=` source (`role R { my $w := $z }`) into
a shared cell when the role is declared. But the body runs at composition, in
the composing scope, and looks `$z` up by name there. When the composer has
its own `$z` (`sub g { my $z = 99; my class C does R {} }`), the bind reached
the composer's variable instead of the declaration site's. It also clobbered
the composer's `$z`, which read back as `Nil`. Rakudo answers `1` and `99`.

The role registration now records the declaration-site cells on the role
(`RoleDef::body_bind_cells`). Each deferred-body statement runs with those
cells in scope under their names, and the composer's own binding is restored
afterwards. The caller-variable write-back the bind records is dropped too,
because the write went to the declaration site's cell, not to the composer's
variable.
