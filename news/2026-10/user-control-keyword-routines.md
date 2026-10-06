User routines named `last`, `next`, `redo`, `proceed`, or `return` now receive
their parenthesized calls instead of those calls being parsed as control flow.
RakuAST lowering applies the same shadowing rule.
