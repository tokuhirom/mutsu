# `@`-array assignment from a lazy user iterator stays lazy

`my @a = Seq.new($it)` and `my @a = $iterable` drained the iterator before
the array existed, so an `is-lazy` iterator that never ends hung the
assignment. The array is now a lazy list that calls the user's `pull-one` on
demand (`CatPullMode::UserIterator`), as in Rakudo; a finite iterator is still
drained into an ordinary array. Found with `Math::Sequences`, whose
`our @A000027 is export = 𝕀` hung `use Math::Sequences::Integer` (#15305).
