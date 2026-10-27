# The invocant and does-target markers are parameter fields

A parameter's `traits` no longer carries the `invocant`, `implicit-invocant` and `does-target`
markers. `ParamDef::is_invocant` is the single invocant flag (about 55 `is_invocant || traits.any(...)`
checks collapse to it), and `implicit_invocant` and `does_target` are fields. The RakuAST conversion
renders `traits` as written without filtering. ADR-10723 section 6.114.
