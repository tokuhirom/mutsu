# RakuAST: `<value:sym<number>>` keeps its adverb on the name

A regex assertion with a `:sym<text>` adverb (the way a grammar calls one candidate of a proto token) crosses the RakuAST boundary the way rakudo renders it: `Assertion::Named` over a `Name` that carries a `ColonPair::Value(key => "sym", value => QuotedString<words val>)`, with no `args`. Lowering turns the colonpair back into the subrule argument the matcher already understands, and the regex source regenerated from the tree prints `<value:sym<number>>` instead of the call `<value(sym<number>)>`, so the capture keeps its long name `value:sym<number>`.

The ratchet in `ci/rakuast-frontend-passing.txt` grows by 5 files. The new test is `t/rakuast/rakuast-regex-sym-adverb.t`. `SubruleArgs::sym_adverb` is the one place that decides whether a subrule's arguments are that adverb.
