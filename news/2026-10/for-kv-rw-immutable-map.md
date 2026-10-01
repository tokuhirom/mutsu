# `for $map.kv -> $k, $v is rw` fails the bind on immutable sources

A multi-parameter `for` loop with an `is rw` parameter over an immutable `Map` or
`Mix`/`Set`/`Bag` now dies with `X::Parameter::RW` at the bind, as raku does, instead of
running the body (Map) or dying later with `X::Assignment::RO` (QuantHash). Closes #10398.
