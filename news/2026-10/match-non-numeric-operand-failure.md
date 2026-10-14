# A non-numeric `Match` is an `X::Str::Numeric` Failure as an operand

`Match.Numeric` is `self.Str.Numeric` in Rakudo, so `("ab" ~~ /.*/) + 1` and the `.sum` of a
list holding such a match are Failures. mutsu's `.Numeric` method already answered so, but
the arithmetic operators and `.sum` read the match as `0`. The operators' shared
`str_numeric_operand_failure` check now treats a plain `Match` like the `Str` it numifies
through, and `Any.sum` checks its `Match` elements the same way (#13160).
