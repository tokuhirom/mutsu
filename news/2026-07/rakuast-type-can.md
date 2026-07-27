# RakuAST type objects expose `.^can`

RakuAST type objects and node values now answer `.^can` for the constructors, accessors, and
mutators implemented by mutsu's model layer. The lookup uses the same metadata as
`.^methods(:local)` and `.^method_table`, closing an inconsistency where a field such as
`RakuAST::IntLiteral.value` was listed by introspection but could not be discovered with `.^can`.
