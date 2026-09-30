# `%?RESOURCES` is a `Distribution::Resources`; subscript routines are assignable

Working `Distribution::Resources::Menu` (ecosystem lock board #10045):
`t/01-tests.rakutest` went from 0 to 2/2.

- `%?RESOURCES` now reports the type `Distribution::Resources` (carried as the
  hash's declared type, like `Map`), so `has Distribution::Resources $.resources`
  accepts it instead of failing with "expected Distribution::Resources but got Hash".
- `postcircumfix:<{ }>(%h, $k) = v`, `postcircumfix:<[ ]>`, and the
  multi-dimensional `{; }` / `[; ]` routine forms are assignable, lowering to the
  same index-assign nodes as the subscript syntax.

The routine call form `postcircumfix:<{; }>(%h, @keys)` as an rvalue is still
unresolved. The record is not re-measured (no rakudo here); run
`ecosystem-sweep.yml` scope=only after merge.
