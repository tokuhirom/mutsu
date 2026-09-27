# `for @a` reads the array in place instead of copying it at entry

A `for` loop used to copy an Array into a `Vec` before its first iteration, twice: once in
`value_to_list`, and again when the loop body built its item list. So `for @a { last }` cost
O(e) however few iterations ran, and a `for @big { ...; last if ... }` inside an outer loop cost
O(outer × e).

Now a plain Array (or List) bound one element per iteration by a sequential loop is iterated
in place. `exec_for_loop_body` takes the array itself, and a `ForItemIter::Live` reads element
`i` of the live array at iteration `i`. Nothing is copied at entry. As with Rakudo's Array
iterator, the body sees an element it pushes, stores or shifts before the loop reaches it:
`for @a -> $v { @a[2] = 9 if $v == 1 }` yields `1 2 9`, the same as `raku`. Element aliasing,
writeback, `last`, collecting loops and gather resumption keep working. A resumption
snapshots the array only when a `take` actually suspends the loop.

Multi-parameter, threaded, itemized, shaped, lazy and native-storage sources keep the
materialized list.

This is part of #9158.
