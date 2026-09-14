# Regex predicate blocks retain their RakuAST shape

`<?{ ... }>` and `<!{ ... }>` regex assertions now retain
`RakuAST::Regex::Assertion::PredicateBlock` in `.AST`, including the nested
block and negation. Constructed RakuAST trees execute through the existing
inline regex-code matcher, so real matches preserve the established
once-per-match behavior.

The interpolated-block and plain code-block forms remain separate boundaries.
