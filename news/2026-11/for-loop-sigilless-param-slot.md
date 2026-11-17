# Sigilless `for` parameters are local slots too

Since #17530 a pointy `for` loop's `$` parameter has been a local slot of the
loop body, so reading it is a slot read rather than a by-name walk of the
environment. A sigilless parameter (`for @a -> \x { ... }`) still went
through the by-name binding; it now gets a slot as well, which takes one more
set of names off the name-keyed path (ADR-12529). An `@`/`%` parameter keeps
the by-name binding for now: a whole-container assignment through a slot
loses a bound object hash's type.
