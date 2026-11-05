# `use v6.c`: a sunk `start` that dies is not fatal

Found by the raku-doc differential sweep (`Type/Promise.rakudoc`, `use v6.c; start { die }; sleep ⅓; say "hello"`).
The fatal "Unhandled exception in code scheduled on thread N" report of a sunk `start` whose
Promise nobody observes is a 6.d feature; under `use v6.c` rakudo prints nothing and the mainline
carries on. The compiler no longer emits the sink mark for a `start` statement under 6.c.
