# `1 [+] 2` keeps its `BracketedInfix` in RakuAST

An operator written in brackets outside `[op]=` lost its brackets in the
RakuAST view: `Q[1 [+] 2].AST` held a plain `Infix("+")` (`.DEPARSE` wrote
`1 + 2`), and `1 ![==] 2` became a prefix `!` over the comparison. The parser
resolves `[op]` to the operator itself, so when an infix application is placed
in the source the view now reads the brackets back from the text between the
operands and wraps the operator in `BracketedInfix` -- nested ones too
(`[[+]]`), and inside a `MetaInfix::Negate` / `Reverse` (`![==]`, `R[+]`).
A negated bracketed operator is now `MetaInfix::Negate` at the application, and
lowering takes the brackets off again, so `EVAL` runs the plain operator
(#18504).
