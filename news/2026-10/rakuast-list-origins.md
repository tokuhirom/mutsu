# RakuAST lists and bracketed statements carry their origins

`.AST` now gives lists and the statements inside brackets the ranges Rakudo
does. An `ApplyListInfix` (`1, 2`, `1 | 2 | 3`) spans its operands, a
trailing comma included, and its `Infix` covers the last operator, as on
Rakudo. The `SemiList` inside `( )`, `[ ]` and an array or hash subscript
covers the text inside the brackets without the surrounding whitespace, and
each `Statement::Expression` in it has its expression's range. A subscript
(`Postcircumfix::ArrayIndex` / `::HashIndex`) runs from the end of its
operand to the closing bracket.

These ranges are derived from the origins of the converted children, and the
source text is checked: each gap between two operands holds exactly the
operator, and a `SemiList` sits between the brackets its node is written
with. A list spread over several lines now reports each of its lines to a
`visit-children` walker.
