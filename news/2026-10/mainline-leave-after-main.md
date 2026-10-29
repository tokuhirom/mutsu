# A file-level LEAVE runs after `sub MAIN`

Rakudo calls `MAIN` from the end of the mainline, so a program's own top-level
`LEAVE` phasers run after it (and before `END`). mutsu ran them when the mainline
code finished, i.e. before `MAIN`, so a header such as Red's
`schema(Model).drop.create; LEAVE schema(Model).drop;` tore its table down before
`MAIN` could use it. When a program declares `sub MAIN`, its top-level `LEAVE`
phasers are now deferred until `MAIN` has returned (#15407).
