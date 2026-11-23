# `.match(/re/, :c(N), :g)` starts its scan at N

`.match`'s `:c(N)` / `:pos(N)` were honoured for a single match but dropped
as soon as `:g`, `:x`, `:nth`, `:ov` or `:ex` asked for several: the scan
started at 0, so `"abc".match(/./, :c(1), :g)` found `a b c` instead of
`b c`. The multi-match scan now starts at the offset, a match that would
cover it from an earlier start is never tried, and `:pos` anchors the first
match (the later ones continue after it), as in Rakudo (#18883).
