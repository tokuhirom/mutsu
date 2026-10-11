# A multi method's `is default` no longer beats a narrower `where` candidate

`is default` on a multi method was consulted among every candidate tied on
literal count and type distance, before the refinement tiers. So
`multi method m(Int $x where * > 0)` lost to `multi method m(Int $x) is
default` for `C.m(5)`, where `raku` runs the `where` candidate (#12775).

`pick_method_winner` now applies `is default` only among the candidates tied
on the full tier order, after the `is_narrower` prune — the rule sub
dispatch already followed.
