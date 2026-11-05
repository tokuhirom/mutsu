# RakuAST: `Name` answers its part queries

`RakuAST::Name` now has `is-identifier`, `is-simple`, `is-multi-part`, `is-global-lookup`,
`is-package-lookup`, `is-pseudo-package`, `is-installable`, `has-colonpairs` and the
`first-part` / `last-part` / `root-part` accessors, with rakudo 2026.09's answer types (`0`/`1` for
the nqp comparisons, a Bool for `is-simple` and `has-colonpairs`). Part of the accessor survey of
#14916.
