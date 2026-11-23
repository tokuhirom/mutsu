# `(@a[0] = 1)++` increments the element

An element assignment yields the element's container, so `++` and `--` applied
to one (postfix or prefix) write that element: `(@a[0] = 1)++` leaves `[2]`, and
`(%h<k> = 1)++` leaves `{k => 2}`. mutsu refused the operand as immutable.

The compiler now expands such an increment into a block. The block binds the
subscript to a temporary once, makes the assignment, then increments the
element through the same subscript. A subscript with a side effect
(`(@a[$i++] = 5)++`) therefore runs once (#18884). This follows #18840, which
did the same for a scalar assignment.
