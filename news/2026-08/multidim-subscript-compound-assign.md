# A multi-dimensional subscript is a compound-assignment lvalue

`@a[$x;$y] += 1` died with `Cannot modify an immutable value`, while the plain
`@a[$x;$y] = $v` form has always worked. The failure was decided at parse time,
not at run time: `build_compound_assign_expr`
(`src/parser/stmt/assign/compound_expr.rs`) has an arm for `Expr::Index` — the
single-subscript lvalue — but none for `Expr::MultiDimIndex`, so every
multi-dimensional LHS fell through to the generic "this is not an lvalue" case
and compiled to an unconditional `__mutsu_assignment_ro` call. The AST dump made
it obvious:

    DoBlock { body: [ MultiDimIndex { .. }, Literal(5),
                      Call { name: "__mutsu_assignment_ro" } ] }

The new arm mirrors the single-subscript one: each dimension is bound to a temp
so a side-effecting subscript (`@a[$i++; f()] += 1`) is evaluated exactly once
and shared by the read-back and the write, then the result is a
`MultiDimIndexAssign` whose value is the compound expression. Short-circuit
operators (`//=`, `||=`, …) keep their existing handling.

Found in `Digest::SHA3`, whose `KeccakF1600` is written almost entirely in this
form (`for ^5 X ^5 -> ($x, $y) { @lanes[$x;$y] +^= @D[$x] }`) — see
`todo/tickets/digest-dist-blockers.md`. An `@`-parameter is bound read-only but
its *elements* are the caller's containers and stay writable, which is exactly
what that code relies on; the RO error was reported against the element write,
so it read like a parameter-mutability bug rather than a missing parser arm.

Pinned by `t/multidim-compound-assign.t` (12 tests, all also passing under
`raku`), covering numeric/bitwise/string operators, three dimensions, an
`@`-parameter and a `multi` candidate, `%h{k1;k2}`, single evaluation of the
subscripts, `//=` short-circuiting, and the value the expression yields.
