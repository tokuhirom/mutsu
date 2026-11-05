# RakuAST: the target of a capture parameter has its range

The `ParameterTarget::Term` of a sigilless capture parameter (`sub f(|c)`, `method m(|args)`) and
the `Name` it wraps now have the range of the name after the `|` (`sub f(|c) { 1 }`: 7-8), as in
`raku` 2026.09. The parser's search for a parameter's target looked for `\c` and never found `|c`.
Test: `t/rakuast/rakuast-capture-parameter-origins.t`.
