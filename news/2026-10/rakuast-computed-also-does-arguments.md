# RakuAST computed `also does` arguments

`also does R[1 + 2]` now round-trips as a `Statement::Also` containing a
parameterized role type whose argument is an ordinary `ApplyInfix`. Conversion
preserves the parser's argument expressions; lowering restores the same
`DoesDecl` consumed by the existing role-composition path. The regression
checks the measured tree and the computed value after EVAL.
