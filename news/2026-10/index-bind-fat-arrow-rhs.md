# Indexed bind takes a `=>` pair as its right-hand side

`%h<k> := $x => 1` parsed as `(%h<k> := $x) => 1`, building a Pair whose key was the internal
`__mutsu_bind_index_value` marker. `:=` and `=>` share the item-assignment tier and associate
right, so the pair is now the bound value. Found by working the `String::Color` distribution,
whose `t/01-basic.rakutest` now passes 12/12 (pinned by `t/lang/index-bind-fat-arrow-rhs.t`).
