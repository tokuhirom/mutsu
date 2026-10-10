# A frame's callable id is no longer an env entry

Every call frame recorded which callable it was running, and a running block
recorded which routine its `return` leaves, as three entries of the frame's
name map (`__mutsu_callable_id` and the block's return-target pair). Every
closure capture copied them as if they were captured names, and every
return-merge loop had to skip them by name so a callee's id did not leak into
its caller.

They are now fields of the env itself, carried to every env derived from it.
Non-local `return`, `leave`, `once` and flip-flops read them the same way as
before. On the FunctionalParsers EBNF parse, closure captures copy 23% fewer
entries. This is the second slice of ADR-12529 phase 1.
