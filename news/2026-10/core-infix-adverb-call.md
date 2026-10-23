# An adverb on a core infix is no longer dropped

`1 + 2 :adverb` silently ignored the adverb (and `say 1 + 2 :adverb` handed it to `say`), and `Str.AST` of the same text died with
"Confused. Two terms in a row". A trailing colonpair now binds to the operator, as in Rakudo: the parser builds the routine call
`infix:<+>(1, 2, :adverb)` for any core infix, so a core operator rejects it with `X::Multi::NoMatch`
(`Cannot resolve caller infix:<+>(Int:D, Int:D, :adverb); none of these signatures matches:`, the core candidates listed), a
user-declared operator receives it, and `.AST` gives `ApplyInfix` / `ApplyListInfix` with the adverb. A junction chain with an
adverb (`1 | 2 | 3 :x`) is one call over all operands. See `t/lang/operators/infix-adverb-core-call.t`.
