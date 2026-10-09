# `.fmt` raises X::Str::Sprintf::Directives::Count on a directive/argument mismatch

`List.fmt` and `Pair.fmt` formatted with whatever the sprintf layer could salvage, so
`<a b>.fmt('%s%s')` answered `a b` where Rakudo throws. Each item (one argument, or two for a
Pair item) is now validated against the format's directive count on the native and the
user-coercion paths, so the error matches Rakudo's. Hash/Set/Bag/Mix `.fmt` is unchanged (Rakudo
accepts a single-directive format there).

Not changed: a scalar non-`Cool` instance (`Date.today.fmt('%s')`) still formats where Rakudo
reports `Cannot resolve caller fmt`. The installed Rakudo also formats a list item through
sprintf rather than calling the item's own `fmt`, so that half of #12391 does not reproduce.
