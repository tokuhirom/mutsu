# `.AST` keeps the parentheses of an assignment written as a call argument

`foo(1, ($s = 5))` now renders the argument as `Circumfix::Parentheses(...)` like
rakudo. The argument parser used the parenthesised-assignment shortcut, which
consumed the parentheses and returned the bare assignment; it now records them as
`Expr::Grouped`, as every other parenthesised argument already does.
