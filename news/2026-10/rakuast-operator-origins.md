# RakuAST operators, ternaries, contextualizers and pairs carry their origins

`.AST` now gives the following nodes the ranges Rakudo does:

- the `Prefix` of `-$x` and the `Postfix` of `$x++`;
- a `Ternary` across its condition and branches;
- the `StatementSequence` inside `$( )` / `@( )` / `%( )`, and the
  statement in it;
- a `key => value` pair inside a hash contextualizer;
- the parenthesized value of a `:key(value)` colonpair.

Each range is derived from origins the node or its children already carry,
and checked against the source text it claims: the operator as written, the
`??` and `!!` between a ternary's parts, the sigil and parentheses of a
contextualizer, and the key and `=>` before a pair's value. A ternary
written over several lines now reports each of its lines to a
`visit-children` walker.
