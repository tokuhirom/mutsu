# A built-in comparison refuses an adverb, as in rakudo

`my $x = 1 < 2 :!foo` failed with "Confused. Two terms in a row", and inside an
`EVAL` the adverb was silently dropped (`1 < 2 :foo` was `True`). A built-in
chaining operator (`<`, `==`, `eq`, `~~`, `eqv`, `===`, `(elem)`, ... and their
`!` negations) takes no adverb; rakudo refuses one at compile time with
`X::Syntax::Adverb`, "You can't adverb &infix:«<»", naming the operator (the
negated base operator for `!eqv`/`!===`). mutsu now does the same; a user
operator that shadows a comparison still takes adverbs (#18452).
