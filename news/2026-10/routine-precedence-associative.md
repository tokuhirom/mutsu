# `Routine.precedence` / `.associative`, also for exported operators

`&infix:<+>.precedence` and `.associative` now answer the `prec` / `assoc` entries of `Routine.prec`
(an empty string for a non-operator, as in Rakudo). The declared precedence is also recorded on the
routine's shared cell, so an operator declared inside `sub EXPORT` and handed out through the
returned `Map` keeps its `is equiv` / `is assoc` traits after the registry entry is gone.
Found by the Understitch distribution (`t/07-properties.t` now passes; all five of its test files pass).
