# A typed parameter runs a core-named subset's `where`

After #19013, `~~`, `nqp::istype` and typed variables ran the `where` of a
subset named like a core type (`subset Int of Any where * > 10`). A sub
parameter typed with it did not: the light call path classified `Int` when
the sub was registered and checked the argument by its tag, so `t(5)` bound
(#19031).

That check now defers to the full type match when the callee's unit declared
such a subset, so `sub t(Int $x)` rejects `5` with `X::TypeCheck::Argument`,
as on rakudo. A program without such a subset keeps the tag check.
