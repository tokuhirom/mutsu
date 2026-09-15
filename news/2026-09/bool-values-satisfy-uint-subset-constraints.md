# `Bool` values satisfy `UInt` subset constraints

`Bool` is an `Int`-based enum in Raku, so both `False` and `True` now satisfy
the non-negative `UInt` subset predicate. This applies consistently to
smartmatches, typed variables, attributes, and `UInt:D` parameters.

Pinned by `t/types/enum-subset/enum-uint-subset.t`. Closes [#8425](https://github.com/tokuhirom/mutsu/issues/8425).
