# RakuAST: attribute defaults keep origins the way Rakudo does

Rakudo gives an attribute's default (`has $.a = ...`) an `Initializer`
origin, and visits it again through the implicit `Trait::WillBuild`, only
when the default is a literal value or a name: a number, a quote with
nothing interpolated (a word quote too), a colonpair, `True`, `pi`. Any
other default (`1 + 2`, `[1]`, `-1`, a block) has no initializer origin and
is visited once, inside the default's thunk (#15780). mutsu placed every
initializer and visited a block default twice. One rule,
`attribute_decl::literal_default`, now decides both, and a non-literal
default's copy under `WillBuild` carries no origins (`origin::clear_tree`).
A public attribute interpolated in a string (`"n=$.n"`, `"@.a[]"`) is placed
too; the string variable placement moves to `string_variable_origins.rs`.
