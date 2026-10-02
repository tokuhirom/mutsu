# `multi f(*%m)` and `multi f()` are ambiguous for `f()`

A slurpy hash never takes a positional, so it no longer counts towards a
candidate's dispatch shape: `multi f(*%m)` and `multi f()` now tie for `f()` and
raise `X::Multi::Ambiguous`, as rakudo does, instead of running whichever was
declared first. The same holds for `($x, *%m)` vs `($x)` and `(*@a, *%m)` vs
`(*@a)`.

Fixing it exposed a second bug in the bare-name dispatch plan: an ambiguity
found in the exact-arity stage did not end the dispatch, so the wider slurpy
stage — where only `(*%m)` remained — went on to pick it. A stage that ends in
an ambiguity (or a `where` that died) now decides the call (#10663).
