# The iterator of `Seq.from-loop(&body)` is an `Iterator` and can be pulled

`Seq.from-loop({ ... })` (no condition) keeps its body in an internal `FromLoopIterator`
instance. `.iterator` of such a Seq failed `Iterator` parameter and attribute checks and had no
`pull-one` (`No such method 'pull-one'`), which broke wrappers such as Qwiratry's
`TypedIterator::Wrapper.new(Seq.from-loop({ ... IterationEnd }).iterator)`. It now matches
`Iterator` / `Iterator:D`, answers `pull-one`, `skip-one` and `sink-all`, and a body that answers
`IterationEnd` (or raises `last`) ends the iterator for good.
