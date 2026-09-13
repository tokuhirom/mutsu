# `enum` accepts an anonymous role literal as a `does` argument

`does` accepts an anonymous role *literal*, not just a role name.
`enum Level <Off Fatal> does role { ... }` (the shape `Lumberjack` and its
plugins use) took the `role` keyword as the name of a role to look up,
found none, and died with `Unknown role: role`, so any distribution
using this shape could not even load.

Two orderings exist, and rakudo treats them genuinely differently:

- `does Role` *before* the value list (`enum E does Role <a b>`) is a real
  declarator trait — the role is actually composed (`.^does` is `True`) —
  but an anonymous role *literal* is invalid even in rakudo there
  ("Invalid typename 'role'"); only a role name is accepted in that slot.
- `does Role` *after* the value list (`enum E <a b> does Role`, the
  Lumberjack/reported shape) is not special enum grammar in rakudo at
  all: `enum E <a b>` parses as an ordinary term (the type object), and
  the trailing `does Role` is the general infix `does` operator applied
  to it and then sunk — it never actually composes anything (`.^does`
  stays `False`, `.^roles(:local)` stays empty), and a second trailing
  `does` even errors as a non-associative operator.

The fix mirrors both exactly: the before-the-values `does` loop
(`parse_enum_trait_clauses`) is unchanged (still rejects a role literal
the same way rakudo does, just via a different error), and a new
`skip_trailing_enum_does_clause` consumes — but discards — a single
`does <Role>` clause after the value list, so parsing no longer crashes
or fragments into a stray `Unknown role: role` statement, without
pretending to compose something rakudo itself does not.

`enum_decl.rs` was split into `enum_decl.rs` +
`enum_decl_traits.rs` to stay under the file-size convention.

See [#8216](https://github.com/tokuhirom/mutsu/issues/8216) and the
regression test `t/oo/role/enum-does-anonymous-role-literal.t`.
