# `"abc".subst-mutate(...)` on a literal throws X::Multi::NoMatch

A literal receiver has no variable for `Str.subst-mutate` to write back to, so it fell through
to the cascades and answered `X::Method::NotFound`. The by-value dispatcher now raises
Rakudo's `X::Multi::NoMatch`.
