# A failed `s///` is `Nil`, not `False`

A destructive non-list `s///` that matched nothing used to evaluate to
`Bool::False`. In Rakudo it is `Nil`, the value of the failed match, so
`my $r = s/q/b/` reads back as `Any` and `s/q/b/ // 'd'` falls through to the
default. The `Subst` opcode now pushes `Nil` in that case. The smartmatch form
(`$x ~~ s/q/b/`) still reports the failure as `False`, as Rakudo does. The
successful `Match` result and the `:g` list result are unchanged (issue #9515).
