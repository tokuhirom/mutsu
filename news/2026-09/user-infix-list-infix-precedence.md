# A user infix `is equiv<Z>` takes the whole comma list

`sub infix:<mp> ($t, *@f) is equiv<Z> is assoc<list> { @f.elems }; say 1 mp 2, 3, 4`
printed `134` where rakudo prints `3`. The operator was declared at the
list-infix level, which is looser than the comma, so its right operand is
the whole list `2, 3, 4`. mutsu parsed `(1 mp 2), 3, 4` instead. The same
happened with `is equiv(&infix:<Z>)`, which PatternMatching's
`infix:<match_pattern>` uses; 14 of its 20 assertions failed (#9405).

There were three gaps:

- **`equiv<Z>` was dropped.** `resolve_infix_symbol_precedence` did not
  know `Z`, `X`, `...` or `minmax`, so the trait resolved to nothing and the
  operator stayed at the default additive level. Those symbols now resolve
  to the list-infix level, `PREC_SEQUENCE`.
- **The comma list was not lifted into the operands.** A custom infix
  parses to the same `Expr::InfixFunc` node as the built-in `minmax`, and
  `minmax` already had a lift across the surrounding comma list. That lift
  now applies to any infixed function at or below the list-infix level. It
  also required three list elements where two are enough, so even
  `5 minmax 3, 2` was `(3..5, 2)` instead of `2..5`.
- **Parenthesized call arguments were never lifted.** `f(0, 1 Z 2, 3)` is
  one argument in rakudo, `(0, 1) Z (2, 3)`, but mutsu passed three. The
  call-argument parser now runs the same list-infix lift as listop
  arguments and parenthesized lists, for `Z`/`X` as well as for
  `minmax` and user operators.

The regression test is
`t/lang/operators/user-infix-list-infix-precedence.t`.
