# `($x = 6)++` increments `$x`

An assignment expression yields the scalar it assigned to, so `++` and `--`
applied to one, in either postfix or prefix form, write that scalar. mutsu
found an increment's target by the shape of its operand. An assignment is not
one of those shapes, so the operand was a plain value and the operator refused
it with "require mutable arguments".

An increment or decrement of an assignment to a scalar (`($x = 6)++`,
`++($x = 6)`, `($ = 1)++`, also for an attribute or a `state` variable) now
runs the assignment and then updates the scalar (#18840).

The element form `(@a[0] = 1)++` needs the subscript evaluated only once and is
filed as #18884.
