# `RakuAST::QuotedRegex.compile-time-value`

A quoted regex node now answers `compile-time-value` with the `Regex` it
denotes, as rakudo does. Red's regex compiler tests match texts against it to
check their SQL lowering.
