# A block-scoped import of a multi family with a proto body binds lexically

A `use` inside a block now binds an imported multi family whose `proto` does
more than `{*}` (`proto f(|) { 'P:' ~ {*} }`) in the block's lexical import
set, as it already did for families with a plain proto. A routine written
outside the block no longer sees the family, and a closure or `&f` taken out
of the block keeps it.

The code object of such a family now runs the proto when it is called where
the family's name no longer finds it, and the proto's `{*}` goes to the
candidates the code object captured. That also fixes `&f` of a block-local
family taken out of its block, which used to skip the proto body
(ADR-8751 stage 2, #8751).
