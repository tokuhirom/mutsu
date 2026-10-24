# A parenthesised or chained assignment is the typed assignment node

The fourth slice of the assignment family (#14948). The parser's
parenthesised-assignment path (`(@a[0] = 5)`, `($x = 1, 3, 4)`) and the
nested assignment it reads inside (`$x = $y = 1`) now build
`ParsedExpr::Assign`, like the statement and expression parsers. The
per-target dispatch the parenthesised path kept as a copy is gone. Its one
lowering is `rakuast::assign_lower`.

An assignment is now a valid target of another assignment, written through
its result container, so `($x = 5) = 6` and `(@a[0] = 5) = 7` render as
rakudo 2026.09 does. Their left side is the parenthesised assignment, not a
call on it.

No `.AST` parse produces the call-lvalue writebacks any more. That let the
conversion drop `call_lvalue_parts`, which read
`__mutsu_assign_callable_lvalue` and `__mutsu_assign_named_sub_lvalue`
calls back into assignments, and the arm it had for a parenthesised
declaration target.

`t/rakuast/rakuast-chained-assignment-node.t` pins the shapes and the
semantics.
