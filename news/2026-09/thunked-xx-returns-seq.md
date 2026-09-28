# A thunked `xx` returns a Seq

`$i++ xx 2` (a call-like left side re-evaluated per repetition, with a small literal or
constant count) was unrolled inline into a `MakeArray`, so it produced a `List` where Raku
answers a `Seq` (`(2, 3).Seq`). The unrolled form now ends in a new `ListToSeq` opcode, so
it agrees with the thunk path and with `1 xx 2`. Pinned by `t/lang/xx-thunk-returns-seq.t`.
