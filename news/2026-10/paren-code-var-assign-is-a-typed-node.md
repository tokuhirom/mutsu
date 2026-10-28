# `(&f = SUB)(ARGS)` round-trips: a `&` variable is an expression assignment target

`assign_lower::is_expression_target` did not list `Expr::CodeVar`, although `lower_expression` already
lowers a `&f` target to an `AssignExpr`. A parenthesised `(&jp = anon sub ...)` therefore missed the
typed assignment node in `paren.rs` and fell to the parser's `__mutsu_assign_callable_lvalue`
desugar, which the RakuAST round trip refuses; `use Needle::Compile` died on its self-replacing stub.
The target is listed now, so the construct takes the same typed node as `($x = 1)` and `(@a = ...)`.
Needle::Compile loads and its suites 01, 03 and 04 match `raku` (02 differs, #15171).
