# `$*CU`: `CORE::Int` resolves to the setting's `Int`

A `CORE::`-prefixed type name had no resolution in the `$*CU` tree. It is now the same
`Declaration::External::Constant` as the bare name (lexical name `Int`, value the `Int`
type object), as in rakudo 2026.11. Fixes #18229.
