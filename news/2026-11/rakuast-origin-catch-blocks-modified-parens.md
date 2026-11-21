# RakuAST origins of CATCH blocks and modified statements in parentheses

`.AST` now places, as rakudo 2026.11 does:

- the `Block` of a `CATCH { ... }` / `CONTROL { ... }` statement, its
  `Blockoid` and its `StatementList`, after the keyword;
- the initializer of an anonymous declaration (`my $ = -3`);
- a statement with a modifier inside parentheses (`(1 if True)`,
  `($c(1.5) if True)`). The parser placed the modifier but not the
  expression before it. That expression's text is parsed again as a
  fragment and its ranges copied over, and the statement then runs from the
  expression to the end of the modifier (#15780).
