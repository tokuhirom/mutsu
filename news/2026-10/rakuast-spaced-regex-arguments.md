RakuAST conversion now retains spaced regex arguments in named assertions,
such as `<lit 'a'>`, as `Regex::Assertion::Named::RegexArg` nodes. Parsed and
hand-built nodes lower through the existing regex execution path, and the
RakuAST frontend passes `roast/6.c/S05-grammar/parse_and_parsefile-6c.t`.
