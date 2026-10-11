# An Array's `$!reified` slots read as Scalar containers

In Rakudo an `Array` keeps a `Scalar` container in every reified slot. So
`nqp::iscont(nqp::atpos(nqp::getattr(@a, List, '$!reified'), 0))` is 1, and a
write to a container bound from that slot changes `@a`.

mutsu stores Array elements bare and hands out an element container on demand.
`nqp::atpos` through the `$!reified` view (its `IterationBuffer` view, which is
only ever made for an Array) now hands out that container: the same shared
cell that `@a[0].VAR` and `nqp::atposref` give. A `List`'s slots still read as
bare values, as in Rakudo. `nqp::iscont` recognises such a cell when it
arrives as a value (#12698).
