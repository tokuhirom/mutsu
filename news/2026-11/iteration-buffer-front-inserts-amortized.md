# `IterationBuffer.unshift` / `.prepend` are amortized O(a)

`IterationBuffer.unshift` and `.prepend` spliced the new elements in at the
front of the element `Vec`, moving every element on each call: O(n + a).
MoarVM's VMArray keeps a start offset, and so does mutsu's `ArrayData` (its
head offset doubles as front slack), but the method rows only had the raw
`Vec` to edit. They now insert through `ArrayData::prepend_values` on the
buffer's own node, amortized O(a) (#17658).

`IterationBuffer.Seq` no longer copies the buffer: it is the same lazy
positional cursor as `List.values`, over the buffer's storage, O(1) to make.
That a pushed element still does not show through a bound Seq is #17938.
