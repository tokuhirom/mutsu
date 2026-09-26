# Array::Sparse is green

A roulette re-measure of Array::Sparse 0.0.13 on `main` (8631c912) found its only baseline file,
`t/01-basic.rakutest`, at parity: 23/23 assertions, up from 22/23. No interpreter change was needed
in this run. The last failing assertion (`is-deeply @a.raku.EVAL, @a`) was fixed by #9615, which
made `eqv` on user instances follow Rakudo's `.WHAT` + `.raku` rule (#9591); that fix is pinned by
`t/oo/attribute/instance-eqv-private-attribute-and-user-raku.t`. The ledger record moves from `red`
to `green`.
