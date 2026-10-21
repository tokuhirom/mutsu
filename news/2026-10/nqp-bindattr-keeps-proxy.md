# `nqp::bindattr` stores a Proxy as the attribute's container

`nqp::bindattr($obj, T, '$!attr', $proxy)` FETCHed a `Proxy` operand before
storing it, like every other nqp operand, so the attribute held the fetched value
and AttrX::Mooish's `AttrProxy` (bound this way for lazy attributes) was never
installed: the builder ran at construction and later reads saw plain values.
The bind ops (`bindattr{,_i,_n,_s}`, `p6bindattrinvres`) now take their operands
unFETCHed, as Rakudo does. With the `is default` fix, AttrX::Mooish's `t/010-base`
and `t/011-arrays` pass in full (#14266).
