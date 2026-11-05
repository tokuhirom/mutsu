# RakuAST: origins of a parameter type with a smiley

A parameter typed `Int:D $x` (`:U`, `:_`) now answers what rakudo's `.AST` answers: the
`Definedness` / `AnyDefinedness` and its inner `Simple` span the whole written type, the `Name`
only the name before the smiley, and the parameter's target declaration visits the same type a
second time, as it already did for a plain `Int $x` (#15780).
