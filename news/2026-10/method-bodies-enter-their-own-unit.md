# Method bodies resolve routine names in their own compilation unit

A sub body and a closure ran in the compilation unit they were written in, so
a routine name they called resolved against their own module's declarations
and imports. A method body did not: it kept the caller's unit, and a call to
a routine its module declared or imported reached the right definition only
through a fallback anchored to the running frame. That fallback was never
memoized, so every such call paid the whole routine-resolution walk.

A method body now enters its own unit too. The routine-resolution memo also
keys a compunit-scoped name by the units of the running frame, so it can record
these answers as well. On the FunctionalParsers EBNF parse, full routine
resolutions fell from 11,581 to 5,383. This is the first step of ADR-12529
phase 2.
