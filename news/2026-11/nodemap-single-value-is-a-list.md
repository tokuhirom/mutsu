# `nodemap` on a single value answers a one-element List

`5.nodemap(*+1)` (and `nodemap` on a `RakuAST::Node`, which is no list) answered the bare
result; rakudo answers `(6,)`, a one-element `List`. Covered by
`t/collections/transform/nodemap-single-value.t`.
