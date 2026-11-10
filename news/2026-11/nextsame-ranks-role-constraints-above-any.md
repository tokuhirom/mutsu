# `nextsame` ranks role-typed candidates above the catch-all

The deferral order used by `nextsame`/`callsame` scored a parameter constrained by a
role (or a `subset` of a role) below an unconstrained parameter, because a role's MRO
has no `Any`/`Mu` tail. A catch-all `multi method m($, $, $)` therefore ran before a
narrower `multi method m(Subset-of-Role $a, ...)`. Role constraints now score at least
as a class does. Found via the Red distribution's `t/006-optimizer.rakutest`.
