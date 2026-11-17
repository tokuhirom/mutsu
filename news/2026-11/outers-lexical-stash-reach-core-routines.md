# `OUTERS::<&name>` and `LEXICAL::<&name>` reach the setting's routines

`OUTERS::` and `LEXICAL::` see every lexical scope out to the setting, so a
`&name` key that no enclosing scope binds names the core routine:
`sub g { OUTERS::<&say> }; g().name` is `say`, as on rakudo. mutsu answered
`Nil` from inside a routine, because both lookups stopped at the program's own
scopes. Now a `&name` key that the scope walk does not find falls back to
`CORE::<&name>`; a lexical or program routine of that name still wins, and a
name the setting lacks is still `Nil`. Part of #17846.
