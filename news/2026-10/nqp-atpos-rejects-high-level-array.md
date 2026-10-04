# nqp positional ops reject a high-level Array

`nqp::atpos`, `nqp::bindpos` and their typed twins now die with rakudo's
`This type (Array) does not support positional operations` when handed a
high-level `Array` instead of reading or writing it. `$!reified` / `$!storage`
now answer a List-kind alias of the same storage node, so the VMArray route
(`nqp::getattr(@a, List, '$!reified')`) still reads and writes through to the
Array. A `List` cannot be told from a VMArray in mutsu and is still accepted.
