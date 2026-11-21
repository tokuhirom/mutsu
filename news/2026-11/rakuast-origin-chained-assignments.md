# RakuAST origins of chained assignments

`.AST` now places the inner assignment of a chained assignment, as rakudo
2026.11 does. In `$x = $y = 1` and `@a[3] = @a[2] = 8`, the parser placed
the outer assignment, but not the assignment on its right or the
subscript's assignee. That inner node now runs from after the outer `=` and
its blanks to the end of the statement, and its parts are derived from that
range.

The placement of an operand the parser left unplaced on either side of an
operator moved into `lone_term_origins`, which keeps `infix_origins` under
500 lines (#15780).
