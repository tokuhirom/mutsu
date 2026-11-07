# RakuAST: class-level `OperatorProperties.infix` / `prefix` / `postfix` / `circumfix` / `postcircumfix`

`OperatorProperties.infix("+")` and its four siblings look an operator up by symbol and answer the
same `OperatorProperties` object the operator nodes give from `.properties` (one shared routine,
`rakuast::operator_props::lookup`). An unknown symbol gets its category's default, the symbol
defaults to the empty string, and the grammar's `< >`, `<< >>` and `« »` subscripts answer the
method-call level as rakudo 2026.09 does (#14916, ADR-14916).
