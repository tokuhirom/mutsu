# `RakuAST::Name::Part::*` answer `name`, `is-empty` and `is-pseudo-package`

The name parts now have the methods their new raku-doc pages document, with
rakudo 2026.11's answers:

- `EmptyEdge.name` is `""` and `EmptyEdge.is-empty` is `True`.
- `Simple.is-empty` is true for an empty identifier.
- `Expression.name` is the value of the literal the part holds
  (`::("foo")` is `foo`), and `Expression.is-empty` is `False`.
- `Simple.is-pseudo-package` is true for the pseudo-packages `MY`, `OUR`,
  `OUTER`, `CALLER`, `UNIT`, `CORE` and the rest, but not for `GLOBAL` or
  `PROCESS`; it used to be false for every name.

mutsu's extra `Name::Part::Empty` class, the pre-2026.11 spelling of
`EmptyEdge`, is kept for now (#18977).
