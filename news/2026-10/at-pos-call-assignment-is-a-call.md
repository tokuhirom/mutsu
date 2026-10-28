# Element stores go through the object's own `AT-KEY`/`AT-POS`

Four related fixes for objects that serve their subscripts through an `is rw`
`AT-KEY` / `AT-POS` returning a Proxy (Red's `ColumnMethods`, t/37-json):

- `$o.AT-POS($i) = $v` is a call whose result is assigned to. The parser used
  to rewrite it into the element store `$o[$i] = $v` (except for a receiver
  spelled `self`), so a user `ASSIGN-POS` written `SELF.AT-POS($index) = $value`
  called itself until the stack ran out. The rewrite is gone; a builtin
  receiver (`Array`, `Hash`, a `Seq`/`LazyList`) now assigns through the
  container the call answers, as a type object already did.
- A key subscript (`.num1<k> = v`, a string index) never falls back to a
  class's `ASSIGN-POS` when it has no `ASSIGN-KEY`; it is served by `AT-KEY`.
- A role mixed in with `but` whose `AT-KEY`/`AT-POS` is the only part of the
  protocol (`$c = Col.new but Meths; $c<k> = v`) is stored through that accessor
  instead of being dropped.
- `X<k>:delete` calls a user `DELETE-KEY` / `DELETE-POS` of the object when `X`
  is a method result (including an `is rw` accessor, which answers a Proxy), a
  sub result or a block topic; the delete used to be dropped silently, so Red
  took `Bla.^all.map({ .num1<ble>:delete }).save` for a projection.
