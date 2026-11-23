# A slot `nqp::setelems` grows in an IterationBuffer is a null

On rakudo, `nqp::setelems` and an `nqp::bindpos` past the end leave nulls in
a VM array's object storage. `nqp::isnull(nqp::atpos($b, 0))` is true, and a
`List` over the buffer reads the slot as `Nil` through `[i]` but as `Mu`
through iteration, `.raku` and list assignment.

mutsu used to fill a grown slot with the `Mu` type object, so `nqp::isnull` was
false. A `bindpos` gap held `Nil` instead, which then iterated as `Nil`
(#17662).

A grown slot is now a hole in the buffer's `ArrayData`: it holds the `Mu`
marker and is left out of the `initialized` record.

- `nqp::atpos` answers null for a hole.
- The `List` view's existing hole rule already reads it as `Nil` for `[i]`.
- `nqp::push` goes through `ArrayData::push`, so a pushed `Mu` is a value,
  not a hole.
- `nqp::shift` of a hole answers null.
- The raw element editor now updates the hole record when it lengthens or
  shortens the storage.
