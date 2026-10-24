# RakuAST: a declared variable's colonpairs are `Name` colonpairs

`my $today:foo<g h>` now shows its `desigilname` the way rakudo 2026.09 does:
`Name.from-identifier("today", colonpairs => (ColonPair::Value(key => "foo",
value => QuotedString(<words val>, "g h")),))`. Each colonpair keeps its
written form: `«a b»` is a `quotewords` quote, `['a','b']` an array
composer, `:a(1)` keeps its parentheses, and a key-less `:<home>` is the quote
alone. mutsu used to keep the whole canonical name (`today:foo<g h>`) as one
identifier.

The variable is still looked up by its canonical name. A `.AST` parse records
the written colonpairs beside the declaration, and `EVAL` canonicalizes the
nodes back with the parser's own rule, so `my $q:foo«a b» = 42; $q:foo<a b>`
round-trips. Found by the roast-wide `.AST.raku` comparison (#7564);
closes #14882.
