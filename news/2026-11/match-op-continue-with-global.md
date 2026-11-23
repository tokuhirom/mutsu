# `m:c(N):g//` starts its scan at N

The `m//` form had the `.match` bug #18883 fixed: `:c(N)` / `:pos(N)` were
honoured for a single match but dropped by `:g`, `:ov`, `:ex`, `:x` and `:nth`,
so `"abc" ~~ m:c(1):g/./` found `a b c`. Those smartmatch arms now scan from
the offset through the same `regex_match_*_from` entry points, and `:pos`
anchors the first match (#18886).
