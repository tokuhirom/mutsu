# Closure sequences: the eager prefix handles Rat elements and mutated `state` rows

The eager warm-up prefix of an unbounded closure sequence now also stops when
an element is a huge `BigRat` or a huge list, and snapshots a returned array:
a generator such as `{ state @row = 2, 1; @row = flat ... }` returns the same
mutated array each step, so all 32 prefix elements used to alias the last row.
`use Math::Sequences::Integer` (sequences A001699 and A005811) now loads
instead of hanging (#15350).
