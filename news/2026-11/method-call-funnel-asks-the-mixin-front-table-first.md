# The method-call funnel asks the mixin front table first

A repeated straight-line role method call on a mixin (`CArray`'s
`ASSIGN-POS` / `AT-POS`) found its remembered method only deep inside
`dispatch_mixin_method_call`, after `call_method_with_values`, its
`_unviewed` and `_inner` layers and a dozen probes that decided nothing for
those keys the first time (`try_var_meta_delegate`, the qualifier split, the
native-backing and user-override checks, `invoke_unadmitted`, ...). The front
table (`caches.mixin_straight_front`) is now asked at the top of
`call_method_with_values_inner` (`Interpreter::mixin_front_hit`), so a hit goes
straight to `run_straight_line_method`. Validity is unchanged: the
composition node, method, definedness and argument type keys, under one
registry write generation, and the lane still declines a call it cannot run.

Callgrind slope on a profiling build (second run, N 500 -> 1500), per
element: `$c[$_] = 1.5e0` on `CArray[num64]` 29.7k -> **25.0k Ir (-16%)**;
fill + `.Array` 71.0k -> **60.8k Ir (-14%)**; a `$c[$i]` read 150.9k ->
145.5k.

Refs #16215.
