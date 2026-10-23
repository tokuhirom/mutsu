# RakuAST infix applications, named calls and initializers carry their origins

`.AST` now places the nodes the parser does not note as terms, the same way
Rakudo does:

- an `ApplyInfix` and its `ArgList` span the operands;
- its operator node covers the operator as written (`Infix`, `Assignment`,
  and the `MetaInfix::Assign` / `::Reverse` / `::Negate` wrappers with the
  inner `Infix` inside them);
- a named call's `Name` covers the name, and its `ArgList` covers the inside
  of its parentheses or the range from its first argument to its last;
- a `my $x = ...` / `:=` initializer runs from its operator to the end of the
  expression;
- the variable a statement-level assignment starts with (`$x = 1`,
  `$x += 1`) has its range too.

These ranges are derived from the origins of the converted children, and
each is checked against the source text it claims. The operator written
between the operands must be the node's own operator, and a call's text must
start with its name. A node the parser made rather than read stays without
an origin, so every range is exact or absent. A `visit-children` walker such
as `Code::Coverable` now sees the line an operator on its own line is
written on.
