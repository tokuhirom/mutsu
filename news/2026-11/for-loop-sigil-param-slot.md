# `for` parameters with `@`, `%` or no sigil are local slots too

Since #17530 a pointy `for` loop's `$` parameter has been a local slot of the
loop body, so reading it is a slot read rather than a by-name walk of the
environment. An `@`, `%` or sigilless (`\x`) parameter still went through the
by-name binding. These parameters now get a slot as well, so `for @rows -> @r`
and `for @a -> \x` read their parameter the same way as `for @a -> $x`. That
removes one more set of names from the name-keyed path (ADR-12529).
