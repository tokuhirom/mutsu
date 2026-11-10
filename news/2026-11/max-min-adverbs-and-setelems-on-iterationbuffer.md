# `.max(:p)`/`.min(:k)` answer a List, work on a Hash, and `nqp::setelems` resizes an IterationBuffer

Three small fixes found through ParaSeq's `t/30-max` .. `t/33-minpairs` (all four now match raku):

- `@l.max(:p)`, `:kv`, `:k`, `:v` (and `min`) answered a `Seq`; rakudo answers a `List`.
- `%h.max(:p)`/`:k`/`:v`/`:kv` answered an empty list (or an index). A Hash is its Pairs: the
  extremal Pairs are the ones with an equal value, and the adverb names their keys, values, both or
  the Pairs themselves.
- `nqp::setelems($iterationBuffer, $n)` did nothing: only a `Buf`/`Blob` and a plain array were
  resized. ParaSeq's `.max(:p)` at degree 3 empties its running result with `setelems($output, 0)`
  per batch, so every later batch piled onto the first one's result. An `IterationBuffer` now
  resizes its element array like an array does; a grown slot reads back as `Mu`.
