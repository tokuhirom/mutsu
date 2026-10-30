# A statement that is a user-declared infix call runs the result's `sink`

`.echo('!!') |> .cat |>> $file.IO;` — Shell::DSL's pipeline statement — builds a `Pipeline` whose `sink`
method runs it. mutsu sank only method calls and `but`/`does` in statement position, so a statement
whose top expression is a user-declared infix (`sub infix:«|>»`, `sub infix:<mine>`) was a no-op and
the redirect never happened. The value of such a call is a fresh object like a method call's, so
its `sink` now runs.
