# A semicolon-subscript bind shares the source's Scalar

`@s[0;0] := $x` (a shaped or a nested array) binds `$x`'s container in rakudo 2026.11: a later
`$x = 5` is seen through `@s[0;0]`, and `@s[0;0] = 7` writes `$x`. mutsu took a readonly value
snapshot instead (`compile_multidim_value_bind` wrapped the right-hand side in `nqp::decont` and
passed no bind sources), and a local test pinned that snapshot in 16 assertions rakudo does not
pass (#18541). The multi-dimensional bind now carries its source like a chained bind does, and
the shaped-array leaf rebinds the source variable to the shared cell it seats. The test was
rewritten to rakudo's behaviour, each assertion checked with `raku`; one assertion (an Array
bound through a chained subscript reads back itemized) is a `todo` for the separate #18566.
