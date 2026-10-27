# A multi candidate whose sub-signature cannot take a list's Pair elements is skipped

Pair elements of a List unpack as the *named* parts of its Capture, so a positional-only
sub-signature (`:$arg! (*@a)`) cannot bind `(getter => &g, setter => &s)`: Rakudo reports
"unexpected named arguments passed (getter,setter) in sub-signature" and dispatch moves on to the next
candidate. The multi matcher ignored the surplus Pairs and picked that candidate, so AccessorFacade's
`is accessor-facade(getter => &g, setter => &s)` trait died with "only takes Callable arguments".
`AccessorFacade` now loads with the named-argument forms; the rest of its suite is #15127 / #15128.
