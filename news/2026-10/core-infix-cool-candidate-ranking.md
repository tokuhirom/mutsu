# A user `Cool` infix candidate no longer ties the core `Rational` one

A user `multi infix:<*>(Cool $v, Int $n)` called with a role-mixin `Rat` was reported
as ambiguous against the core `(Rational:D, Int:D)` candidate. Rakudo answers
`Rational ~~ Cool` (and `Real`, `Numeric`) with True, so `Cool` is the wider type and
the core candidate wins. `builtin_role_ancestors` now lists `Cool` for those roles, so
`nominal_type_conforms` sees the relation (#14575).
