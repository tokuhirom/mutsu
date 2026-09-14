# Interpolated regex code blocks retain their RakuAST shape

`<{ ... }>` regex interpolations now retain
`RakuAST::Regex::Assertion::InterpolatedBlock` and its nested block in
`.AST`. Parser-created and constructed regexes execute the block through the
existing closure-interpolation matcher.

`<!{ ... }>` remains the separate predicate-block assertion form.
