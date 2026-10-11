# `samecase`, `indent`, `parse-base`, `substr-eq` and the other text methods are method-table rows

`samecase`, `samemark`, `indent` and `encode` on `Str` and `Cool`, `parse-base`, `substr-eq`,
`naive-word-wrapper`, `Date`, `DateTime` and `Stringy` on `Str`, and `encode` on `Uni` are now
method-table rows, each sharing one implementation with the dispatch cascade (ADR-11276 §9.74).
The `text` group's remaining rows are the ones that need the interpreter (`subst`, `trans`,
`match`, `indices`, `IO`, `sprintf`, `printf`) plus `split` and `fmt`.

Three answers moved toward Rakudo on the way: `"abcd".substr-eq("bc", 10)` is `False` instead of
an out-of-range `Failure`; `"11".parse-base("2")` is `X::Multi::NoMatch` instead of "no such
method"; and `Uni.encode` (`"aé".NFD.encode`) answers a `Buf`.
