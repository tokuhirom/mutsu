# RakuAST hyper function infix

The remaining RakuAST hyper-function infix gap is closed in both directions.

`>>[&infix:<+>]<<` now renders as
`ApplyInfix(MetaInfix::Hyper(FunctionInfix(Var::Lexical)))`, preserving the
left/right DWIM flags exactly as Rakudo does. `EVAL` lowers the model back to
mutsu's existing `HyperFuncOp` execution path, so strict, left-DWIM,
right-DWIM, and both-DWIM forms round-trip.

The dual-oracle coverage is in `t/rakuast-hyper-function-infix.t`.
