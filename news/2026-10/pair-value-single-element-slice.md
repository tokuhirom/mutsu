# `key => @a[1..1]` keeps a one-element slice a List

A pair whose value is a one-element array slice (`k => @a[1..1]`, `k => @a[1,]`) collapsed
to the bare element, because the container-mode subscript read the one-item list or range as
a single position (its string form parsed as an integer). `index_to_usize` now refuses a
list or range, so such an index is a slice selector. Found with Markdown::Grammar, whose
`t/05-Raku-section-tree.rakutest` now passes.
