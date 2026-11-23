# A one-entry hash composer with a trailing comma is a list in RakuAST

`{:k,}`, `{a => 1,}` and `%(:k,)` now view their entry inside the comma list
the trailing comma makes, `HashComposer(ApplyListInfix(",", (ColonPair::True("k"),)))`,
with the list and its comma placed, as rakudo parses them. `.DEPARSE` keeps
the comma and `EVAL` gives the same hash. The first half of the issue (the
terms of `{:f(1, 2)}`) already matched rakudo (#18444).
