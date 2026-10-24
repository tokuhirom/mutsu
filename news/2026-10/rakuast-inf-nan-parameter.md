# `sub f(Inf)` renders as a literal-value parameter in RakuAST

`Inf` and `NaN` in a parameter position are literal values, which rakudo
2026.09's tree renders as a `Num` parameter with `value => Inf`. mutsu's tree
had a parameter of a type named `Inf` (#14885). The conversion and the
lowering now map between that tree and the parser's type-only parameter that
dispatch already treats as the value.
