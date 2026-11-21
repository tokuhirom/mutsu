# RakuAST origins of a bound subscript

`.AST` now places the subscript a binding assigns to (`@a[0] := $v`,
`%h<q> := 1`), as rakudo 2026.11 does. The parser placed the binding and
its value but not the postfix application on its left. That left operand
now runs from the start of the binding to the blanks before `:=`, and its
variable and subscript are derived from that range. Only a postfix
application is placed this way: a term the parser unwrapped from
parentheses is not its own text (#15780).
