# `:16(<78>)` converts an allomorph's string

`:N(...)` (`UNBASE`) accepts a `Str`, and an allomorph such as the IntStr
`<78>` is one: rakudo converts its string, so `:16(<78>)` is 120 and
`:16(<1e2>)` (a NumStr) is 482. mutsu refused anything that was not a plain
Str ("Cannot convert 78 to a radix-16 number: please supply a Str value"),
which killed Compress::Zlib::Raw's `t/02-native-stream.t` at
`<78 9c ...>.map({ :16($_) })`. The allomorph's string part is now used.
