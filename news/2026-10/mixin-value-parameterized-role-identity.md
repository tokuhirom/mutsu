# `.^mixin` of R[1] and R[2] are different types

The composition key of a mixin recorded only the type of a role's value argument,
so `Any.^mixin(R[1])` and `Any.^mixin(R[2])` shared one `.WHAT` node and
`.^set_name` on one renamed both. The value is part of the key now, as in Rakudo.
