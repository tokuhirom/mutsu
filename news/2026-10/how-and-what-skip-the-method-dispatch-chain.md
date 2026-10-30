# `.HOW` and `.WHAT` skip the method dispatch chain

Rakudo compiles the unquoted `.HOW` and `.WHAT` as macros, so no user method can sit between the
call and the reflection answer. mutsu still ran them through the whole method-call chain: a miss
in the native probe, then `call_method_with_values_inner` and its receiver gates, and only then
`dispatch_how` / `dispatch_what`. `Foo.HOW` cost 38.3k instructions per call.

`CallMethodMut` now answers an unquoted, argument-free `.HOW` / `.WHAT` on a type object or an
instance directly (`try_mop_reflection_lane`), unless a `.wrap` chain or a user `find_method`
exists. A quoted call (`Foo."WHAT"()`) still reaches a user method.

Measured with callgrind on a 2000-iteration `Foo.HOW` loop (second run, profiling build, loop
count pinned, startup subtracted): 38.3k -> 6.1k Ir per call. It does not move
`Red::Column.new` (118.7M -> 120.1M Ir over 50 constructions, i.e. noise): Red's `.HOW` calls do
not go through this opcode, so `t/38-equal.rakutest` (#15498) is unchanged by this slice.
