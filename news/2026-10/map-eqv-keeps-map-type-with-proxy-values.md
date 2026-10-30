# `eqv` of a Map holding Proxy values stays a Map

`Map.new((a => $proxy)) eqv Map.new((a => 1))` was False (raku: True). To read
the Proxies' FETCHed values, `eqv` rebuilt the hash with `Value::hash`, which
dropped the `Map`'s declared type, so it then compared a `Hash` against a `Map`.
The rebuild now keeps the hash's metadata (declared type, key type, itemization).
`annotations::containers::Keeper.hash` (a Map over Proxy slots) needs this for
`%@keeper eqv Map.new(...)`.
