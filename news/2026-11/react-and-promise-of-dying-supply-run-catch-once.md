# A `CATCH` around `react` or `.Promise` of a dying `supply` runs once

The `die` of a `supply` body ran a surrounding `CATCH` at the throw site (the
body is called inline), and again when the error unwound or the failed promise
was awaited, so `{ await supply { die }.Promise; CATCH { ... } }` ran its handler
twice and merely making the Promise already threw. The body is now run with the
outer `CATCH` handlers hidden, as nested VM runs already are (ADR-0072), and
`wrap_react_died` carries the inline-`CATCH` verdict over to the
`X::React::Died` it builds (#16054).
