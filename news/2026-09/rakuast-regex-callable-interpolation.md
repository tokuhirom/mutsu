# Callable interpolation in regex ASTs

The RakuAST converter now retains argument-less callable interpolation in
regexes, including both `<&name>` and `<&name()>`, as
`RakuAST::Regex::Assertion::Callable` with its lexical callee. Empty callable
parentheses have the same AST shape as the omitted parentheses. Calls with
non-empty argument lists remain a separate follow-up boundary.
