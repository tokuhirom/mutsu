# A pointy block's capture parameter takes a sub-signature

`-> | ($, $v) { ... }` and `-> |c ($a, $b) { ... }` parse and unpack the captured arguments, as
`sub f(| ($a, $b))` already did (annotations' `Keeper` uses the first). The other half of the
ticket, `my @m = LIST.map(&proxy-maker)` keeping the Proxy containers, was fixed in between; a
regression test pins that `my @m = ...` stores the values.
