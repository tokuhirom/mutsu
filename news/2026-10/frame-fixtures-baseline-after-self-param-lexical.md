# The frame-fixtures baseline names the two `self` spellings #15476 added

The `check-frame-fixtures` ratchet (#15467) and the `$self`-parameter-as-lexical change
(#15476) were open at the same time, so `main` failed the ratchet once both landed.
The two new references are name tests, not `Env` reads or writes of the fixed names:
`types/signature.rs` maps the spelling `self` to the reserved `LEX_SELF` lexical for
a plain routine's own `$self` parameter, and `methods_classhow_arms_4.rs` gained one more
`"self"` comparison. Their per-file counts under `scripts/frame-fixtures.d/` are raised
to 1 and 5.
