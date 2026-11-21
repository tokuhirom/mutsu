# RakuAST origins of trait arguments and hash-contextualizer colonpairs

`.AST` now places, as rakudo 2026.11 does:

- the colonpairs inside a hash contextualizer, `%(:a(1), :b<c>)`, read back
  from its text the way a `{ ... }` composer's are, with the statement
  around them;
- a routine trait's parenthesized argument (`is DEPRECATED("x")`,
  `is export(:DEFAULT)`, `is Tag(:tag<m>)` for a user type trait). The text
  between the parentheses is parsed again as a fragment, and its ranges are
  copied onto the argument (#15780).
