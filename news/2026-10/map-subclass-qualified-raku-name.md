# `Map::raku` on an `is Map` subclass names the subclass

`$x.Map::raku`, `$x.Map::gist` and `Map.^lookup('raku')($x)` on an instance of an
`is Map` subclass now render with the subclass name (`V3.new((:a(1)))`) instead of
`Map.new(...)`, matching rakudo. Found working the `immutable` distribution
(`ValueMap`); its remaining failure is the `is Pair` subclass gap, #12169.

A user `method raku` on a scalar-held `is Map` subclass is also used as-is now,
without the `$(...)` itemization marker the core renderer adds around its own text.
