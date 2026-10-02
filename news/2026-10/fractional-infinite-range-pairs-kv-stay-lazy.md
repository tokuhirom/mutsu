# `.pairs`/`.kv`/`.antipairs` over a non-integer infinite range stay lazy

The lazy index pipe for `.pairs`/`.antipairs`/`.kv` over an infinite range
used to cover integer starts only (`1..*`, `^Inf`). A Rat or Num start
(`(1.5..*).pairs`, `(1e0..Inf).kv`) still reified a 100,000-element prefix,
which took ~0.9 s to answer `.head(2)`, and `.is-lazy` said `False`.

`LazyList::index_pipe_method` now uses any infinite range with a numeric start
directly as the pipe's source. `pull_source_element` already steps such a range
one element at a time and keeps the start's type (`1.5..*` gives Rats).
`-Inf..Inf` repeats `-Inf` as Rakudo does. The integer-only
`infinite_int_range_sequence` split is no longer needed and has been folded
back into `infinite_int_range_to_lazy_array`. `(1.5..*).pairs.head(2)` now
takes ~10 ms.

A `Str` range (`'a'..*`) is still reified eagerly.
