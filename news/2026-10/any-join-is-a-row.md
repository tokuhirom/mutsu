# `Any.join` is a method-table row

`join` on `Hash`, `Map`, `Pair`, `Range`, `Capture`, `Match`, scalars and the temporal classes now goes through the `Any.join` row, whose
one body (`join_core`) replaces two diverging cascade arms (ADR-11276 §9.63). `\(1, 2).join` answers `"12"` as Rakudo does.
