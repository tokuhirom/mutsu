# `Parameter` accessors are visible to `.can` and `.^methods`

The accessors of a `Parameter` (`name`, `type`, `optional`, `slurpy`, `sub_signature`,
`signature_constraint`, ... 26 in all) were plain instance attributes, so calling them worked but
`.can`, `.^can` and `.^methods` did not report them. `Parameter` now has an opaque dispatch shape
and one table row per accessor (`method_table/instances/parameter.rs`), which read the same
attributes. `nominal_type`, `of`, `coerce_type`, `is-item`, `onearg` and `untyped` are still missing
(#15660).
