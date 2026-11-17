# RakuAST: hash literal keys keep their quotes

A hash literal entry whose key was a quoted string or a number lost how it
was written. The parser stores a literal-keyed hash as `(key, value)` pairs,
so RakuAST rendered every entry as a bare `FatArrow`. `{"a b" => 1}` even
deparsed as `{a b => 1}`, which does not parse back. Rakudo keeps such an
entry as an `ApplyInfix` over the written key (`QuotedString`,
`IntLiteral`).

The parser now marks the value of such an entry with
`Spelling::HashPairKey`, which holds the key as written and the pair's form.
The view renders the pair the way it renders the same pair outside a hash
literal. Lowering marks the entries again and accepts numeric keys. Found
through the `.DEPARSE` corpus (`integration/advent2010-day08.t`, #14880).
