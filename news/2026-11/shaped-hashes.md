# Shaped hashes: `my %h{Str;Int}` keyed by a list

Rakudo 2026.11 makes an object hash declared with a key type per dimension a
shaped hash (`Hash::Shaped`): `my %h{Str;Int}` is keyed by a list holding a
`Str` and an `Int`. mutsu used to keep only the first key type, so
`%h{"a";1}` built a nested `%h<a><1>` and answered `(5)`. It now does what
rakudo does (#19283):

- `%h{"a";1}` reads, writes, tests (`:exists`) and deletes (`:delete`) the
  one entry the key list `("a", 1)` names. A compound assignment
  (`%h{"a";1} += 2`, `//=`) works the same way.
- Each key is checked against its dimension's type (rakudo's
  `X::TypeCheck::Binding`: "Type check failed in binding to 'key of
  dimension 2'"). A subscript with too few or too many keys raises
  `X::NotEnoughDimensions` / `X::TooManyDimensions`.
- `.^name` is `Hash[Any,(Str,Int)]`, `.shape` is `((Str) (Int))`, `.keyof`
  is `List`, and `.keys` / `.pairs` give the key lists. `.raku` renders
  `(my Any %{Str;Int} = ("a", 1) => 5)`.
- `my Int %t{Str;Str} = ("x", "y") => 3` files each pair under its key list.

The hash is an object hash whose key type is `List`. Its declared type
`Hash[V,(K1,K2)]` carries the dimension types, and each entry is stored
under the joined `.WHICH` of its keys, as rakudo's `shaped-key` does
(`value::shaped_hash`). The object hash re-keying step now uses that same
joined key for a shaped hash.

A subscript with fewer keys than dimensions (`%h{"a";"b"}` on a three
dimension hash, or `%h<a>`) names every entry whose key list starts with
those keys: a read gives their values, `:exists` one `True` per entry (empty
when none), `:delete` the removed values, and `:k`/`:v`/`:kv`/`:p` shape the
matches. Still to do: `++`/`--` on a multi-dimensional subscript (#19318).
