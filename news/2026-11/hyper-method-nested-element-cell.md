# `».method` over a nested list of slot cells dispatches on the cell's content

A hyper method call recursing into a nested list handed an element that is an array-slot
cell (`($x[0], $x[1])`, the elements of a Match's capture array) to the leaf lookup as the
cell itself, so Match-only methods were not found: `(($x[0], $x[1]),)».made` died
`No such method 'made' for invocant of type 'G'` (`.ast`, `.from` the same; `.Str` and
`.keys` happened to work). The top-level loop already opened such a cell; the recursive
path now does too, writing an in-place mutation (`>>++`) back through it. Net::NetRC's
actions (`...map({ ... })».made`) work: both of its test files match `raku`.
