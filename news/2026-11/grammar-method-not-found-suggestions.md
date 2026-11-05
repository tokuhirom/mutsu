# A user grammar's "Did you mean" tip offers the Match names

`G.TOP` on a grammar without a `TOP` rule now ends `Did you mean 'to'?` and `G.parse("x")` does too,
as rakudo's `X::Method::NotFound` does: a user grammar is a `Grammar`, a `Match` and a `Capture`, and the
methods Rakudo declares on those take part in the suggestion. The missing-start-rule error also offers the
grammar's own rule and method names. Follow-up to the user-class suggestions of #16782 (found by the
raku-doc differential sweep, `Language/grammars.rakudoc`).
