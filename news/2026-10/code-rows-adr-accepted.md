# ADR-12523 is accepted: `Code`'s methods are rows without a dispatch shape

Every method Rakudo declares on `Code` (`of`, `returns`, `arity`, `count`, `signature`, `line`,
`file`, `name`, `Str`, `Capture`, `clone`) is now a method-table row reached through its owner,
and ADR-12523 is accepted with that outcome: no `DispatchShape::Code` was needed.

`gist` and `raku` turned out not to be `Code` methods in Rakudo (they are `Routine`'s and
`Block`'s), so they stay in the by-name dispatch, now as two named routines in
`methods_code_render.rs` instead of inline blocks of `methods_sub.rs`.
