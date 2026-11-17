# `my %m is Map = a => Nil` keeps the `Nil`

`is Map` already kept a `Nil` value from a list-of-Pairs initializer (#12478).
It now also keeps one from a single Pair initializer, written literally
(`my %m is Map = a => Nil`) or held in a variable. A literal Pair is minted as
the data-flavoured `ValuePair`, which the check that looks for `Nil` keys did
not recognize, so the `Nil` became the `Any` of a Hash container.
