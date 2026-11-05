# RakuAST: parameters of an anonymous `sub` / `method` have their ranges

`sub ($a, |c) { }` and `method ($x, $y?) { }` carried no range on their parameters, targets or
signature. They now match `raku` 2026.09: each `Parameter` and its target, and the `Signature` from the
first parameter to the end of the last (the parser already recorded the parameters' positions; the
anonymous-routine conversion never placed them). A method literal's parameters skip its receiver.
Left: the body's `Blockoid` range and the empty signature's range (`sub () { }`: 5-5), neither of
which the parser records. Test: `t/rakuast/rakuast-anon-sub-parameter-origins.t`.
