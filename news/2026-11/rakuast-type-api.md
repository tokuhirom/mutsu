# `RakuAST::Type::*` introspection

The type nodes now answer what their new raku-doc pages document:

- `Type::Capture.new($name, :smiley<D>)` and
  `Type::Capture.from-identifier("T", "D")` build `::T:D`. `.definite` is
  `True` for `:D`, `False` for `:U`, and the `Bool` type object without a
  smiley.
- `Type::Simple.is-native` and `Type::Setting.is-native` are true for a native
  type (`int`, `num64`, `str`, ...).
- Every type node has `.is-coercive`, which is true only for a
  `Type::Coercion`.
- `Type::Definedness.name` is the smileyed name (`Int:D`).
- `Type::Enum.new` takes no `name` (an anonymous enum, `our`-scoped) and any
  term node.

That a parsed `::T:D` parameter drops its smiley is tracked in #19020 (#18978).
