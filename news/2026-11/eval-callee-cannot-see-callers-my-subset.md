# A callee's EVAL no longer sees its caller's `my subset`

A `my subset` declared in a routine body keeps its bare name in the global subset registry, so a
plain sub the routine calls could resolve it from its own `EVAL` (`EVAL q[my Small $x = 5]`).
The registry now records routine-local subsets with the stack depth they were declared at, and the
`EVAL` of a plain sub declared outside every routine takes the ones its dynamic callers declared
out of the registry for the length of the `EVAL` (the mechanism `my sub` already used). The
declaring routine's own `EVAL`, a closure a helper runs and a mainline-block subset are unaffected
(#16099).
