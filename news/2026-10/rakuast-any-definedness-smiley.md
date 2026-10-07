# RakuAST: the `:_` smiley type is `Type::AnyDefinedness`

`.AST` of `Int:_`, `int8:_`, `Any:_.WHAT`, a parameter `Int:_ $x` and a variable `my Int:_ $x` now
renders rakudo's `RakuAST::Type::AnyDefinedness.new(base-type => Type::Simple(...))`, next to `:D`
and `:U`, which stay `Type::Definedness`. The node converts, lowers back to the parser's `T:_`
constraint string and runs through `EVAL`; before, the bareword form was refused outright.

Four more `t/` files now pass under `MUTSU_RAKUAST=1` (the nativecall smiley tests and
`definite-type-object.t`). Hand-building `Type::Definedness` / `Type::AnyDefinedness` with `.new`
is still unsupported (neither class resolves as a constructor); that is #12290.

This is a slice of the S10 residual work of #7564.
