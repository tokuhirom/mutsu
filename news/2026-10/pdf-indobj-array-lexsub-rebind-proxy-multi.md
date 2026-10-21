# PDF `indobj-array`: a rebind in a method's sub, and a Proxy argument to a multi

Two bugs sat behind `t/indobj-array.t` test 9 of PDF 0.6.17.

A named sub declared inside a *method* that rebinds a variable of the method
(`$val := ...`) replaced only its own env entry, so a sibling sub, or the
method, never saw the new binding once the sub ran after the method returned.
The variable gets a binding cell now when the sub rebinds it, as it already did
for a plain routine.

A multi dispatcher value called from another compunit with a `Proxy` argument
ranked its candidates on the FETCHed value but matched them on the Proxy
itself, so a `%a` candidate lost to the `Mu` one. Candidates that take the
caller's container (`is rw`, `is raw`, `\x`, `|c`) still match the Proxy;
every other candidate matches what the FETCH answered.
