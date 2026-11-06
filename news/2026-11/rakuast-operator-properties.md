# RakuAST: `properties`, `reducer-name` and `default-operator-properties` of operator nodes

`Infix`, `Prefix`, `Postfix`, the subscripts, `DottyInfix::Call`, `FunctionInfix` and the
`MetaInfix::*` nodes answer an `OperatorProperties` object (precedence, associativity, `dba`,
`thunky`, `iffy`, ...) and the `&METAOP_REDUCE_*` name of their fold, from the built-in precedence
table plus the operators the grammar parses without a routine. Every table symbol was probed
against rakudo 2026.09, including the few it does not know (ADR-14916, #14916).
