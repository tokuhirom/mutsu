# A `does Sequence does Positional` class is subscripted through its iterator

`$seq[0]`, `$seq[*-1]` and slices on a class that composes `Sequence` and `Positional` with only an
`iterator` (Red's `ResultSeq`, also under a `but` mixin) answered the whole object as a one-element
list. They now read the delegate's `List`, as `Any.AT-POS` reads `self.list`. Red t/38 `position
zero/last/missing` and the `any`/`one` junctions depend on it.
