# Multi-method dispatch follows rakudo's `is_narrower` partial order

`multi method` candidates are now ranked with the same incomparability relation
multi-sub dispatch already used (#11943): when one candidate wins a positional
parameter by a refinement (`where`, literal, subset) and another wins a different
parameter by a narrower nominal type, they are incomparable and the first one
declared wins, instead of the type-distance sum or the literal count deciding.
Both rankers now record their per-parameter profile through one
`RankProfile::record` and compare with one `RankProfile::incomparable`, and an
explicit `Any`/`Mu` parameter is related to every type there. This fixes
CSS::Writer 0.3.3's frequency round trip (`t/write-css.t` test 72).
