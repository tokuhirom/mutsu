# Four more `VarDecl` pseudo-traits are `VarMarks` fields

`__sigilless_decl` (a `my \x` term), `__method_assign_decl` (an expression-position `my Foo $x .= new`),
`__begin_static` (a declaration a lifted BEGIN gave a static cell) and `__begin_static_definite` (the
static half of a `:D` scalar) were `__`-prefixed entries of a declaration's `custom_traits`, found
again by string comparison in the compiler, the EVAL lexical scan, the self-initializer check and the
RakuAST converter (which had to special-case one of them as "internal"). They are
`VarMarks::{sigilless_decl, method_assign_decl, begin_static, begin_static_definite}` now.

Deleted: the four trait-name constants, the BEGIN prologue's trait copy/retain bookkeeping around
the `:D` half, the converter's `is_internal` exemption and every `custom_traits.iter().any(…)` scan
for these names.
