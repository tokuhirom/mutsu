# Role-composed submethods now flatten into the consuming class at every non-6.e revision

`compose_role_into_class` gated ALL role-composed submethods (not just
BUILD/TWEAK/DESTROY) behind a stale `class == 6.c && role == 6.c` check, so
at the default (6.d) language revision a role's `submethod` — public or
private — never landed in the consuming class's own method table. A private
submethod called via `self!name` from the class's own method body raised
"No such private method", and a public one raised "No such method", even
though `raku` composes both normally at 6.c and 6.d.

The real rule, worked out against `raku` across the whole revision matrix
(including cross-module compositions with differently-versioned classes and
roles): BUILD/TWEAK/DESTROY stay gated on `6.c && 6.c` (they run through a
separate role-submethod-ordering walk regardless, so composing them into the
flat table too would double-invoke them for a 6.e+ class); a role's PRIVATE
submethod always composes, at every revision; every other (public,
non-construction-phase) submethod composes at 6.c and 6.d but not at 6.e+ --
`roast/S14-roles/submethods-6e.t`'s own comment already recorded this ("Since
6.e submethods are not composed into consuming classes"), and
`roast/S14-roles/versioning.t`'s "Submethods" subtest pins the same rule with
`^submethod_table` across every class/role revision pairing.

This also unblocks `Math::Matrix::Util`'s private helper submethods
(`!check-row-index`, `!check-column-index`, ...), one of the two blockers
recorded in #8815.

Pinned by `t/oo/role/role-submethod-composed-into-class.t`.

Part of #8815 (bug 1 of 2; see the companion entry for bug 2).
