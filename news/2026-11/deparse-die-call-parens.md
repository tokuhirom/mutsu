# `die(...)` / `fail(...)` are calls over their arguments

`die("S")` was read as the listop `die` over a parenthesised argument, so
`.AST` showed a `Call::Name::WithoutParentheses` over a `Circumfix::Parentheses`
(`die ("S")`, and `die(("S"))` inside an expression). With the parenthesis right
after the name it is `Call::Name` over the arguments, as in raku; with a space
(`die ("S")`) it stays the listop. Several arguments now make one message
(`die("a", "b")` dies with `ab`). `fail("S")` as a statement stays the listop form for now (see the TODO in `control_stmts.rs`).
