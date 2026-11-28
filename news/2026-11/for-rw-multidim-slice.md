# An rw `for` over a multi-dimensional slice writes back

`for @b[1;*] <-> $x { $x *= 10 }` (and the topic form, `for @c[*;0] { $_++ }`)
iterated copies of the selected leaves, so the writes never reached the array.
The `for` lowering now hands an rw (or topic) loop over a positional
multi-dimensional slice the leaf containers, through the same
`MultiDimIndexBindRef` op the associative `%h{*;*}` slice already used, so the
loop variable aliases each leaf as rakudo does (#19339).
