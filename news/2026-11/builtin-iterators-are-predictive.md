# Built-in list, array, range and buffer iterators are a `PredictiveIterator`

`().iterator ~~ PredictiveIterator` answered `False` in mutsu, so a sub with a
`PredictiveIterator:D $i --> PredictiveIterator:D` signature (Sequence::Generator's
`all-but-last`) rejected the iterator of an empty or reified source. The dispatch shape already
told a predictive `Iterator` instance from a lazy stream, a `squish` scan and an unending source,
but the type check compared class names only. `type_matches_value` now asks that shape for the
`PredictiveIterator` constraint, and a hash's iterator is marked plain (`not_predictive`), as in
rakudo. Still different: `.WHAT` is `Iterator` (rakudo names `Empty`, `ReifiedListIterator`, ...),
and the iterators of a `squish` Seq and of `Hash.keys` are predictive here (an eager list under
the Seq) but plain in rakudo. Test: `t/collections/lazy-seq/predictive-iterator-builtin-membership.t`.
