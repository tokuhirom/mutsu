# `Seq.new($iterator).elems` asks the iterator's `count-only`

`.elems` on a `Seq.new($iterator)` nothing had pulled reified the whole Seq and
drained the Array or Range iterator behind it, so a second `Seq.new($iterator)`
over the same iterator (Iter::Able's `n'th`, called twice on one iterator) read
`Nil`. The count now comes from the iterator's `count-only` and neither the Seq
nor the iterator is advanced, as in Rakudo. Iterators without a predictable
count (`.map`/`.grep` streams, `squish` scans) are still counted by running
them. Iter::Able `t/17-n'th` now matches raku.
