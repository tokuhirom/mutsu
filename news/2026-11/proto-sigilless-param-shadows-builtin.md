# A proto body reads its sigilless parameters as terms

`proto f(\item, *@p) { "{item}-{@p}" }` called the builtin `item` instead of reading the
parameter: the proto body was parsed without the sigilless/callable parameter terms that a
plain `sub` body registers. It now registers them, so `f(5, 1)` is `5-1` as in rakudo (#16933).
