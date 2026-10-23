# The metamethods of a `.^mixin` type object read its mixin class

`Class.^mixin(R)` (and `Class but R`) is a type object over a registered mixin class `Class+{R}`,
but `.^method_table`, `.^add_method`, `.^can` and the other metamethods that read a type's members
used the base class (`Class`): `$m.^method_table<attr>` was `Any`, `$m.^add_method(...)` died with
"add_method target must be a type object", and a method added to the mixin class was invisible to
`^can`. They now use the mixin class (`Interpreter::mixin_class_of_type_object`). AttrX::Mooish
installs its `:alias` accessors with exactly this call; its `t/080-aliasing` `but/does` subtest
still fails further on (assigning through the alias of a lazy `rw` attribute).
