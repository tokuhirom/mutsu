# `:v` now reads uncached lazy sequences

Subscript value adverbs such as `:v` now force the prefix required by their
index when the target is an uncached `LazyList`. This fixes `Text::Markov`'s
`read()` method, which applies `:v` to the lazy sequence returned by `reader()`
and previously received an empty list instead of the generated values.

The force remains bounded by the subscript index, so an indexed read does not
materialize an otherwise unbounded lazy source.
