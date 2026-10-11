# A thunked `xx` returns a Seq

`($i++ xx 2)`, `(rand xx 2)` and the other re-evaluating forms of `LHS xx N` with a
small literal count are unrolled in the current frame, and the unrolled result was built
with `MakeArray`, so it came out as a `List`. A new `ListToSeq` opcode turns it into the
`Seq` Rakudo returns (`(2, 3).Seq`). Pinned by `t/collections/xx-thunked-seq.t`.
