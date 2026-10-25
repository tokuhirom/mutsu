# A nested or term-form DECLARE'd class keeps the metaobject its shell's traits built

A class declared as a term (`given model C is table<x> { }`) or inside another declaration is registered
by a compile-time shell, which runs its custom `is` traits once, and again in place. The in-place
registration replaced the shell's metaobject with a fresh one and re-mixed only the roles' bare names, so
what a trait had stored in the metaobject (Red's `is table<...>` sets `$!table`) and the arguments of a
role it had mixed in (`$type.HOW does Carries[Arg]`) were lost: `given model BlaBleBli2 is table<not_that> { }`
reported the default table name. The in-place registration now keeps the shell's metaobject.

This reverts the transplant of only the shell's mixin onto a fresh instance (#15016), which fixed a failure of
Red's `t/61-model-as-type` that turned out to be an unrelated, non-deterministic one (#15020): a recursive
frame's `my %pars` shared with a nested call once a thread has spawned. The test pins that a nested and a
term-form class are composed exactly once with the shell's metaobject kept, and that a trait-stored value
survives.
