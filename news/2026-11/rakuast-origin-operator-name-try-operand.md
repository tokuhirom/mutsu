# RakuAST origins: an operator name's word quote and a prefix's expression statement

`sub term:<forty-two>` / `multi trait_mod:<is>` give the `QuotedString` and `StrLiteral` of
their `<...>` an `origin`, and `try EXPR` / `do EXPR` give the `Statement::Expression` they
hold the origin of its expression, as rakudo 2026.11 does. A 34-file differential over
`t/rakuast/*.t` (every 25th file) went from 26 to 30 files identical at the `from`/`to`
level (#15780). `.origin.Str` remains separate (#18312).
