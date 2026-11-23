# Spaced `** :? N` regex quantifiers

A `**` range takes its backtracking modifier before its bounds, and rakudo
lets blanks surround it: `/ a ** :? 2..3 /` is a frugal `a**2..3`. mutsu's
regex parsers read a modifier only directly after the `**`. The spaced form
therefore matched nothing (`Nil` where rakudo gives `｢aa｣`), and its `.AST`
failed with "literal Regex without a declarator tree". Both the regex source
tree and the matcher now skip the blanks, so the spaced form matches,
converts to the same RakuAST as rakudo's, and round-trips. (#18967)
