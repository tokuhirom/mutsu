# `IterationBuffer.List` views the buffer's storage; `nqp::clone` of a buffer copies it

`$buffer.List` copied the elements, so a List taken before an in-place `nqp::shift` / `nqp::push`
went stale: Sequence::Generator's `LambdaN` (a sliding window of the last N values handed to a
WhateverCode generator) repeated its last value, `1, 1, 2, 4, 8, 15, 15, 15, ...` instead of
`... 29, 56, 108` (#16889). In Rakudo the List is built over the buffer's own storage, and the
List now is too. Conversely `nqp::clone($buffer)` shared the storage node with the original, so a
push onto the clone grew both; the clone now owns a copy of the elements.
