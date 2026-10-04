# A user type named `Cursor` is its own type

`Cursor` is still a core alias of `Match`, as in rakudo, but the alias no
longer wins over a program's own declaration. `grammar Cursor { … }`,
`class Cursor { … }` and their `my`-scoped forms now dispatch their own
methods and type-check as themselves, where mutsu used to resolve the name to
`Match` and die with "No such method 'TOP' for invocant of type 'Match'"
(#11705). The alias lives in one place,
`Interpreter::resolve_core_type_alias`, shared by bareword resolution and type
matching.
