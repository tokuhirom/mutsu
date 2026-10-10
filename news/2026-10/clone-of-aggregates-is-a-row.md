# `clone` of Array, Hash and Pair is a method-table row

`Array`, `Hash` and `Pair` now declare `clone` as `Handler::Interp` rows that share one copy routine with the
dispatch cascade (ADR-11276 §9.62). The row carries a typed container's declared element/key type over to the copy, and the
duplicate by-name array arm and `Value::array_shallow_clone` are gone. No behaviour change.
