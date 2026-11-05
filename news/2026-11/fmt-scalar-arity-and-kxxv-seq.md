# `.fmt` of a scalar dies with rakudo's X::AdHoc; `Bag.kxxv` is a Seq

`3.fmt("%s %s")` and the other single-value `.fmt` calls with a wrong directive
count now raise a plain `X::AdHoc` with rakudo's short message, as `List.fmt`
already did, instead of the typed `X::Str::Sprintf::Directives::Count`
(`sprintf` and `Pair.fmt` keep that one). The `Count` message's hint has one
space before "Are you using", as in rakudo. `Bag.kxxv` answers a `Seq`.
