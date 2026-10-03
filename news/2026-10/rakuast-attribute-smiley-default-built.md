# RakuAST: attribute smileys, `is default`, `is built` and `my role`

After the parameterized-role slice, two refusals stood out. 63 `t/` files
stopped at "role with custom traits", nearly all of them a `my role`, whose
`my` the parser records as an internal `__my_scoped` marker. Another 102
stopped at an attribute with a trait, a smiley or a scope. Measured on rakudo
2026.09:

- `my role R { }` is a `Role` with `scope => "my"`, as `my class` already was.
- `has Int:D $.x` / `has Str:U $.u` type the declaration with
  `Type::Definedness`; lowering splits the smiley back off the base type.
- `is default(EXPR)`, `is built` and `is built(False)` are `Trait::Is`, the
  first and last with a parenthesized `argument`.

A scalar attribute with `is default(3)` and no initializer starts with 3, so
the parser also stores 3 as its `default`. That made `has $.y is default(3)`
indistinguishable from `has $.y is default(3) = 3`, and the first rendered an
initializer the source never wrote. Following ADR-10723's rule for a spelling
the parser normalizes, `HasDecl.default_is_trait` now records where the value
came from.

`Trait::Is` declares its `name`, `argument` and `type` fields, so `.argument`
on a bare trait answers a type object, as in rakudo.

`handles` and `Int:_` still decline. So does an attribute with more than one
trait, because the parser does not keep their source order.

The round-trip ratchet grew by 64 files, to 3163.
