# A bound `@a.values` Seq sees a later push

`my $s := @a.values; @a.push(4); $s.elems` is now `4`, as on rakudo, and the
same holds for an `IterationBuffer.Seq`. A Seq iterates its source only when
it is consumed. Before, the store took a snapshot of the Seq: it read the
Seq's length while checking whether a one-element Seq of `Nil` should
collapse to `Nil`. That read made the Seq's elements final. A Seq whose
cursor nothing has read yet now skips that check, which also keeps
`[Any].values` the Seq it is.
