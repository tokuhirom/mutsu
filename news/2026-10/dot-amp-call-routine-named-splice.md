# `$x.&splice(...)` in a `map` block calls the declared routine

String::Splice's test calls its own `proto sub splice` as `$s.=flip.=&splice($s.chars/2, "$_")`
inside `(^10).map: { ... }` and died "Routine does not have any candidates. Is only the
proto defined?" (28 ok under rakudo, 8 under mutsu). `$x.&f(args)` compiles as the direct
call `f($x, args)` (#15623), but the parser keeps the list-operator names (`splice`, `push`,
`shift`, `say`, ...) apart when a program declares or imports a routine of that name, as
`UserRoutineCall`; the rewrite used a plain `Call`, which inside a `map` block resolved the
core list routine instead. `.&name` always names a routine, so for those names the rewrite
now builds the same `UserRoutineCall` the written-out call becomes.
