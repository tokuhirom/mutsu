# A Seq element of a real Array renders bare in `.raku`

`my @f = $seq; @f.raku` printed `[$((1, 2).Seq),]`; raku prints `[(1, 2).Seq,]`.
The store leaves the element as `Scalar(ItemSeq)`, and the element renderer
printed the inner Seq's own `$` on top of the Scalar it already de-itemizes.
It now renders the bare Seq (#17117).
