# A call above a later `use` reaches an imported proto whose candidates are not registered by name

`my $*RED-DB = database "SQLite"; ... use Red;` (Red `t/23-metamodel-model.rakutest`, line 3)
died with "Routine does not have any candidates. Is only the proto defined?" since #15231
moved the late-import fallback behind the function lookup: `Red::Database`'s exported
`proto database` is found by name, but its multis live in the module's scope. A `{*}`-only
proto with no registered candidates now yields to the routine a later top-level `use`
imports (#15246).
