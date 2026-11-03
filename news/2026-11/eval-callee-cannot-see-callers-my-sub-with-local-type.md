# A callee's EVAL no longer sees its caller's `my sub` that mentions a local type

A `my sub` declared in a routine body that also declares a `my class`/`role`/`enum`/`subset` it
mentions stays registry-resolved (it is not a frame lexical), so it stayed in the flat routine
registry while the routine's callees ran and a callee's `EVAL q[helper(5)]` could call it. The
registry now records which routine-local subs it holds and at what stack depth; an `EVAL` run by a
plain sub declared outside every routine takes the ones its dynamic callers registered out of the
registry for the length of the `EVAL`. The declaring routine's own `EVAL`, a sub or closure nested
in it, and ordinary calls are unaffected (#16099).
