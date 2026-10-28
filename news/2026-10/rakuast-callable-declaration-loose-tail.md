# RakuAST: `my &code = EXPR or die ...` round-trips

A code-variable declaration followed by a loose-precedence tail (`my &decoder = %!decoder-for{...}
or die ...`, from `Protocol::Postgres`) was refused with "a declaration with a loose tail". The
parser seeded the tail's re-read of the declared variable with a plain `Var("&decoder")` instead of
`CodeVar("decoder")`, so the converter never saw the declaration as the tail's leftmost operand.
`seed_read_expr` now knows the `&` sigil. Covered by
`t/rakuast/rakuast-callable-declaration-loose-tail.t`.
