# RakuAST: `ArgList` answers `arity`, `has-args` and `arg-at-pos`

`RakuAST::ArgList` now has `arity` (an Int), `has-args` (a Bool) and `arg-at-pos`, whose negative
index counts from the end and which answers `Mu` past either end, as on rakudo 2026.09. Part of
the accessor survey of #14916; `from-comma-list` and `unshift` are still missing.
