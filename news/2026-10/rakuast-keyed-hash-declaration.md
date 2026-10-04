# RakuAST: key-typed hash declarations `my %h{Str}`

After the allomorph slice, the "coercion type" refusal led the `.AST`
survey with 75 `t/` files. The message was the converter's catch-all for a
type string it could not read. Naming the type in the message showed that
most of them were key-typed hashes (`my %h{Any}`, `my Int %h{Str}`). The
parser folds such a declaration's value and key types into one string,
`"Int{Str}"`.

Measured on rakudo 2026.09, the key type is the declaration's `shape`, a
`SemiList` holding the type. A written value type is its `type`:

```raku
VarDeclaration::Simple(type => Type::Simple(Int),
  shape => SemiList(Statement::Expression(Type::Simple(Str))), …)
```

The parser spelled a missing value type as `Any`, so `my %h{Str}` and
`my Any %h{Str}` were the same tree. Following ADR-10723's "a spelling the
parser normalizes gets a flag", it now also records an
`__implicit_value_type` marker on the declaration when it supplied the
`Any` itself. The compiler skips it like every `__` trait. The new
`ast::keyed_hash` module owns the marker and the fold.

- The converter splits the string into `type` and `shape`, and leaves the
  `type` out when the marker is there.
- Lowering folds a `shape` back into the string and restores the marker.

Lowering a `shape` on anything but a `%` declaration with one key type now
refuses instead of silently dropping it. `RakuAST::SemiList` also gains the
`.statements` accessor rakudo gives it. The catch-all refusal now names the
type it could not read.
