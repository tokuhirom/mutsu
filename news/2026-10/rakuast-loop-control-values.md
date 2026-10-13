# RakuAST: loop controls with a value or a slip round-trip

`last $_ * 10 if ...`, `next Empty` (v6.e) and `next |$c` as statements no
longer stop the `MUTSU_RAKUAST=1` round trip: lowering accepted only a label
argument. A value or a slip now lowers to the same expression the parser
builds, a bare word counts as a label by the parser's own rule (so `next Empty`
stays a value under v6.e), and a slipped capture keeps the call form that lets
it carry a label. Four more `t/` files pass in that mode.
