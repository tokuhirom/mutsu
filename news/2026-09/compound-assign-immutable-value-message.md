# Compound assignment to a non-container value names the value

`@a[i]:v += x`, `%h<k>:v ~= x` and `f() += 1` (non-rw `f`) now raise
`X::Assignment::RO` with Rakudo's `Cannot modify an immutable Int (10)` message
instead of a generic one. Internal `__mutsu_*` call LHSs route through
`__mutsu_assign_callable_lvalue`, and a non-rw sub reports the value it returned
(also for the plain `f() = 3` form).
