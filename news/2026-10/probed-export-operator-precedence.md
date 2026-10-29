# An operator probed from an export stash keeps its precedence trait

A module that copies another package's symbols into its `EXPORT` stash at run
time (`OUR.WHO.BIND-KEY: .key, .value for Ops::`, as the `annotations`
distribution does) is run by the importer's parse to learn its exported
names. The probe recorded only the names, so an exported operator declared
`is looser<...>` parsed with the default additive precedence and `Foo graffiti
6, 7` became `(Foo graffiti 6), 7`. The probe now also reads each operator's
`Routine.prec` hash and hands the parser its level and associativity.
