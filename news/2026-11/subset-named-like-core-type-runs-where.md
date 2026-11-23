# A subset named like a core type runs its `where` for a native value

A subset may reuse a core type's name (`subset Int of Any where ...`). It then
shadows the core type in the unit that declares it. mutsu's core scalar type
check answered `5 ~~ Int` and `nqp::istype(5, Int)` by the value's own type
before the subset was consulted, so the `where` never ran (#19013).

When the program declares such a subset, that fast check now steps aside for
the subset's name. This applies only in the declaring unit's code: a
module's `Int` is still the core type there. That matters because subsets are
registered by name, and Test.rakumod's `Int` parameters would otherwise run the
program's `where`. `nqp::istype`, `~~` and typed variable stores now run the
`where`, as on rakudo.

Typed parameters (`sub t(Int $x)`) still take the light call path's
registration-time check; #19031 tracks that.
