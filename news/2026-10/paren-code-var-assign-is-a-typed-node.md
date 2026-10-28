# `(&f = SUB)(ARGS)` round-trips: a parenthesised `&` assignment is a typed node

A parenthesised `(&jp = anon sub ...)` missed the typed assignment node in `assign/paren.rs` (the
expression target set has no `&` variable, on purpose: `my $a where &even = 6` must leave the `=` to
the declaration) and fell to the parser's `__mutsu_assign_callable_lvalue` desugar, which the RakuAST
round trip refuses; `use Needle::Compile` died on its self-replacing stub. `paren.rs` accepts a
`&` target itself, since inside parentheses it is always an assignment, so the construct takes the same
typed node as `($x = 1)` and `(@a = ...)`. Needle::Compile loads and its suites 01, 03 and 04 match
`raku` (02 differs, #15171).
