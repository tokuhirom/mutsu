# `$self` parameters of loops, map blocks and `is rw` parameters are user lexicals

#15476 moved a plain routine's `$self` parameter to the reserved `$self` key. This finishes the
move (#15464): `ParamDef::storage_name` is that key for every `$self` parameter, so an `is rw`
`$self` writes back, a block-local `my $self` shadows without clobbering the parameter, and a
`for -> $self` loop and `.map(-> $self { ... })` bind it as a lexical too. `Compiler::resolve_self_lexical`
and the `self_is_signature_param` flag are gone; `MY::<$self>` no longer lists the `self` term.
