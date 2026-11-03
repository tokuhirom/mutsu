# RakuAST: origins of statement-call arguments that share a shape

The arguments of a statement-level call whose terms are alike (`is(2, 2, "x")`, `foo 1, 1`) had no
origins: the conversion made copies of the positional arguments, and a copy finds its range only
when no other term of the statement has its shape. The arguments are now converted where they sit
(`stmt_call.rs`), so every `Test` call with a repeated literal is placed. `view.rs` shrinks by one
line (its ceiling is lowered). Part of #15780.
