# `S///` autothreads over a Junction topic, and junctive `&` binds tighter than `|`

`S/x// given $junction` substituted inside the Junction's stringification and
returned a Str; it now substitutes each eigenstate (nested junctions included)
and returns a Junction of the same kind, as `.subst` does (#15627).

`1 | 2 & 3` was parsed left-to-right as `all(any(1, 2), 3)`; the junctive `&` is
tighter than `|` and `^`, so it is `any(1, all(2, 3))` (found with
Data::Dump::Tree's `1 | 'a' & True`). The `?|`-style boolean-bitwise operators
keep their previous grouping.
