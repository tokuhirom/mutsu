# `.DEPARSE` no longer dies on four missing RakuAST accessors

Sampling the `.DEPARSE` corpus showed ~8% of `t/` files aborting inside the vendored
`RakuAST::Deparse` on accessors mutsu's nodes did not answer (#14845):

- `Type::Definedness.through-pragma` (every `Int:D` / `Str:U` smiley; False when the
  source wrote it),
- `Type::Subset.where` (a subset without a `where` clause),
- `Native.repr` (an undefined `Str` unless `is repr(...)` was written),
- `Nqp.args` is an `ArgList` node, as in rakudo, instead of a plain list.

The accessor rows were regenerated with `scripts/gen-rakuast-accessor-rows.py`.
