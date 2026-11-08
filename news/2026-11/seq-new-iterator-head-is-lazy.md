# `Seq.new($iterator).head(n)` stays lazy

`.head(n)` on a `Seq.new($iterator)` pulled its `n` values at the call, so a consumer that only
wanted to build the Seq (BinaryHeap's `extract(n)`, whose Seq removes heap elements as it is
pulled) mutated its source immediately. It now returns a Seq over a `HeadIterator` that shares a
remaining-count with the original iterator, pulling nothing until the result is consumed and exactly
`n` values then, as rakudo does. BinaryHeap `t/01-binaryheap.rakutest` matches raku.
