# Empty reductions now preserve `X::NoZeroArgMeaning`

Reducing an empty list with an operator that has no identity element, such as
`[x] ()`, now returns a `Failure` wrapping `X::NoZeroArgMeaning`. The exception's
`name` attribute contains the operator's long name, matching Raku.
