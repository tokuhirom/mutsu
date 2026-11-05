# RakuAST: `Parameter` answers `named`, `is-positional`, `is-optional` and `multi-invocant`

`RakuAST::Parameter` now answers the four questions rakudo 2026.09 does, computed from the node's
fields (`src/rakuast/parameter_queries.rs`; rows in
`src/builtins/method_table/instances/rakuast_parameter_queries.rs`). The rules were measured on
rakudo over 18 signatures (scalar, `?`, default, `:$x`, `:$x!`, `is required`, slurpies, a capture,
a sigilless parameter, a renamed named parameter, a parameter after `;;`): `named` is a non-empty
`names`; `is-positional` is neither named nor slurpy; `is-optional` is the explicit `optional`, else
false for a slurpy, true for a named parameter or one with a default; `multi-invocant` is true unless
`multi-invocant => False`. Test: `t/rakuast/rakuast-parameter-queries.t`.
