# Constant colonpair statements and `:7a` keep their colonpair form in RakuAST

`.AST` refused a constant colonpair standing as a statement (`"{:a(7)}"`,
`@{:a}`, a bare `:a;`): the parser folds it to a `Pair` value, keeping only its
source text for the sink warning ("literal Pair"). The converter now parses
that source text again with the parser's colonpair routine and renders the
written `ColonPair::Value` / `True` / `False`, as rakudo 2026.09 does.

A numeric colonpair (`:7a`, `:3rd`) rendered as a `FatArrow`; it now has its own
`BinaryForm::ColonPairNumber` and renders as `ColonPair::Number`, both as a
statement and as an argument.

`t/rakuast/rakuast-colonpair-literal.t` pins the tree and the EVAL direction;
`t/lang/malformed-syntax-classes.t` joins the round-trip ratchet with it.
