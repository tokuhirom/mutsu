# RakuAST origins: definedness terms, radix parentheses, valued adverbs, lookarounds

Four more `.origin` families now match rakudo 2026.11 (#15780):

- a `Type::Definedness` written as a term (`$x ~~ HyperWhatever:D`): the
  inner `Type::Simple` spans all of it and the `Name` the name before the
  smiley, as for a parameter's type;
- the parentheses of a radix number with an expression (`:16($n)`): the
  `Circumfix::Parentheses`, its `SemiList` and statement;
- a subscript adverb with a value (`%h{$k}:exists(1)`, `%h<a>:delete(True)`):
  the `ColonPair::Value` and its parentheses, so the subscript's own range,
  which runs over its adverbs, is placed too;
- a lookaround (`<?before <$x>>`, `<!after \d>`, `<?before 'a'>`): the
  `Assertion::Lookahead`, the `Named::RegexArg` inside it, its `Name` and its
  argument.

On a sample of every 25th `t/rakuast/*.t` (34 files) the walk is now
identical to rakudo's in 33; the 34th does not compile on rakudo.
