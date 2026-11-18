# A superscript power is `Postfix::Power` in RakuAST and `.DEPARSE`

`2²`, `$n⁻¹` and `(1 + 2)³` were parsed straight into `**`, so `.AST` showed an `ApplyInfix` and
`.DEPARSE` printed `2 ** 2`. The parser now keeps the spelling (`Spelling::SuperscriptPower`), and the
view builds rakudo's `ApplyPostfix(operand, Postfix::Power(power))`; the class and its `power`
accessor row are new, and lowering turns the node back into the same `**`, so `EVAL($ast)` is
unchanged.
