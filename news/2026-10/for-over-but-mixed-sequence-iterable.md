# `for` over a `but`-mixed Sequence+Iterable instance iterates its elements

`for (Seqy.new but Extra)` handed the loop the drained list as one item. The class's own
`iterator` of a `but`-mixed `does Sequence` instance now runs with the mixed value as `self`
(`call_user_iterator_on_mixin`, as `.list` already did), and `try_user_iterator_items` unwraps a
`Mixin` the way `try_iterable_instance_items` does. A `does Sequence` instance also keeps the
default `.raku` (`Seqy.new`) instead of the list's: only `Seq` has a `raku` of its own in rakudo,
the `Sequence` role does not delegate it (#14921).
