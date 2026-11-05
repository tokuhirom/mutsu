# `$x is copy` shares the Array it is bound to

A scalar `is copy` parameter gets a fresh Scalar around the same Array or Hash, as in Rakudo:
`sub f($x is copy) { $x.push(9) }` reaches the caller's array. The general binder deep-copied the
aggregate (so `sub f($x is copy, $y = $x.elems)` and the light call path disagreed); only an `@` or
`%` copy parameter copies it now (#15649).
