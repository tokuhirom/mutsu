# `.Seq` of an unpulled map/gather no longer runs it

`Seq.Seq` is the receiver itself in Rakudo. mutsu reified the body at the
`.Seq` call, so a `map` or `gather` body ran too early and in the wrong dynamic
scope. It now returns the Seq untouched (#15755, #15669).
