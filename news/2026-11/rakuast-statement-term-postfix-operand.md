# RakuAST: a parenthesized statement called on keeps its parentheses

Rakudo drops one level of parentheses around a postfix operand, but not
around a conditional or a `for` / `given` statement written as a term:
`(for <a b> { .uc }).join` is an `ApplyPostfix` over
`Circumfix::Parentheses(SemiList(Statement::For))`, while `(do for ...).join`
is one over `StatementPrefix::Do`. mutsu viewed both as the `do` form
(#15780). `Expr::postfix_operand` now keeps the parentheses around such a
statement. A statement-control statement in expression position, which the
parser keeps no range for, is now placed too: over the text inside its
parentheses, or from after a prefix's keyword (`do for ...`) to its end
(`control_statement_origins.rs`).
