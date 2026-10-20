# A bound array takes the whole right-hand side of `(*, @a) = ...`

`my @b := [0, 0]; (*, @b) = 1, 2, 3, 4` left `@b` as `[2 3]`. The first array
of a list assignment slurps every remaining value, whether or not it was
bound with `:=`, and rakudo gives `[2 3 4]`. mutsu had recorded each array
bind's element count under a name-derived env key,
`__mutsu_bound_array_len::@b`, and truncated the right-hand side to it.

The key and the truncation are gone (ADR-0097, #8087). The bind's
synthetic block now signals bind context with `Stmt::MarkBind`, as a hash
bind's does, instead of a call that wrote the key.
