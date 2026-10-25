# A bracketed metaoperator assignment to an element is the typed node

`%h<k> [R//]= $v`, `@a[$i] [X+]= $v` and `@a[$i] [+]= $v` move onto
`ParsedExpr::CompoundAssign` (#14948), like the same assignments to a variable
(#14982). PostCocoon::Url uses the first form: `$result<port> [R//]= ~.<port>`.

The node's lowering (`compound_lower::bracketed_meta_expression`) stores the
metaoperator's value through the subscript, with `build_meta_assign_expr`'s
expansion.

`.AST` used to render the element form as an `assignee` folded into the
literal hash index. It now gives
`ApplyInfix(left => the element, infix => MetaInfix::Assign(BracketedInfix(MetaInfix::Reverse(Infix("//")))))`,
as rakudo 2026.09 does. `rakuast::lower` reads it back into the same node.

The parser builds the node through `written_bracketed_meta_assignment` at two
sites:
- the operand path in `logic`;
- the parenthesized path in `paren`.

`t/rakuast/rakuast-subscript-meta-assign-node.t` pins the shapes and the
results.
