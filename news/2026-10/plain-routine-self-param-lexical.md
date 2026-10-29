# A plain routine's `$self` parameter is a user lexical

A parameter spelled `$self` in a plain routine, pointy block, `for` or `map`
block now binds the reserved `$self` lexical key (`ParamDef::storage_name`),
exactly as methods already did, instead of the `self` key the `self` term used
to share. The compiler's `resolve_self_lexical` mapping, the
`self_is_signature_param` flag, the binder's mirror and the explicit-`$self`
branch of `function_invocant_arg` are gone, so
`method m { sub ($self) { $self ~ self.^name }("x") }` prints `xD` as in raku
(#15464, ADR-14843).
