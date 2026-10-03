# `join` runs the callbacks of nested deferred `.map` Seqs

An outer `.map` whose block returns another `.map` Seq left the inner Seqs
deferred, and `join` — both the `.join` method and the `join` function, which
flattens them — rendered each one from its still-empty seed. So
`(1, 2).map({ (3, 4).map({ 7 }) }).join('&')` gave `&` instead of
`7 7&7 7`, and URI::Query::FromHash's `hash2query` returned an empty query
string for every input. `.join` now stringifies such an element through its
`.Str`, and the `join` function reifies the nested deferred Seqs it flattens
before joining. URI::Query::FromHash's test suite passes.
