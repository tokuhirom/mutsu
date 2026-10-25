# Statement-level assignment to a method call, call or list is the typed node

A statement `TARGET = SOURCE` whose target is a method call (`$o.a = 5`), a
call (`f() = 3`), a callable (`$c() = 1`) or a `*`-slurpy list (`(@a, *) = …`)
used to be expanded inline by the statement parser into
`__mutsu_assign_method_lvalue` / `__mutsu_assign_callable_lvalue` /
`__mutsu_star_lvalue_rhs` shapes, which `.AST` then had to recognise and undo.
These statements now build the same typed `Assign` node as every other
assignment, and `assign_lower` is the only place that expands it (the
`*`-slurpy list keeps its statement form there).

Deleted: `rakuast::star_lvalue` (the `__mutsu_star_lvalue_rhs` reverse
recognizer), `convert::method_lvalue_parts`, `convert::method_lvalue_assignment`
and the three `convert` arms that called them, plus the statement parser's own
method, call and callable expansions in `simple_expr_stmt/core.rs`.
