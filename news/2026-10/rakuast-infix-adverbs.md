# Adverbs on a user-declared infix round-trip through RakuAST

`sub infix:<foo>($a, $b, :$round) { … }; 1 foo 3 :round` passes `:round` to the
operator. `.AST` refused it, because the parser keeps the adverb as one more
operand of the operator call. It now renders the application as rakudo
2026.09 does, `ApplyInfix(left, Infix("foo"), right, colonpairs =>
(ColonPair::True("round"),))`, and lowering hands the adverbs back to the
operator call, also for a declared core operator such as `infix:</>`.

`t/rakuast/rakuast-infix-adverbs.t` pins the tree and the EVAL direction;
`t/lang/operators/operator-adverbs.t` joins the round-trip ratchet with it.
