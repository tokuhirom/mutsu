# A gather passed to a builtin routine is read the way Rakudo reads it

`sum(gather { take 1; take 2 })` answered `0`, `max` returned the whole list,
`set` built an empty Set, and `uc`/`chrs` produced the empty string. A builtin
routine got a plain gather as its inner coroutine list, which most of the pure
builtins read through its still-empty cache.

The builtin argument boundary now reads a gather argument per routine, as
measured against Rakudo (ADR-11678 §8.2). Streaming routines (`first`, `zip`,
`roundrobin`, `flat`, `map`, `grep`) still pull an infinite gather only as far
as they need, and they consume it. Iterating ones (`sum`, `max`, `unique`,
`classify`, `cross`, ...) pull it whole and consume it, so a second read throws
`X::Seq::Consumed`. Every other routine (`elems`, `join`, `sort`, `set`, the
string coercions) reads the gather through its cache and leaves it readable. A
Seq that an earlier read consumed now throws `X::Seq::Consumed` when it is
passed to another builtin, as in Rakudo.
