# RakuAST: an assignment to a parenthesised list

After call-lvalue assignments started rendering, 39 `t/` files were still
refused under `__mutsu_assign_callable_lvalue`. Most of them, 28, were a
plain list assignment like `($a, $b) = 1, 2` or `($a, @rest) = @input`.

The parser builds such an assignment as a write-through on the list
container: `__mutsu_assign_callable_lvalue(ArrayLiteral(LVALUES), [], rhs)`.
Rakudo 2026.09 renders it as `ApplyInfix(Assignment)` whose left side is the
`Circumfix::Parentheses` list. The converter now does the same, and the
`.AST` text matches rakudo's.

The parser had two places that built `(LVALUES) = rhs`: the plain list, and
a list-valued call target. Both now go through one
`parser::paren_list_assign_expr`. Lowering calls the same function, so the
round trip keeps both behaviours. A lone target among `*` placeholders
(`($x, *) = …`) takes one item. Any other list assigns through the list
container.

The other 11 files are other left sides: a ternary, a `do` block, and the
internal targets of the compound forms. They come from other parser paths
and stay refused.
