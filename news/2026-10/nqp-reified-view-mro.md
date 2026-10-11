# The `$!reified` view answers IterationBuffer's MRO

`nqp::getattr(@a, List, '$!reified').^mro` is `(IterationBuffer, Any, Mu)` as in Rakudo,
not `List`'s chain (#12700). The view and `IterationBuffer.new` instances are still two
representations: `nqp::bindattr` of storage onto an instance re-points the instance's
slot, which a view over a fixed storage node cannot do (see the comment on #12700).
