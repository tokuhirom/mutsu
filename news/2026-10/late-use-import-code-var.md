# `&name` above a later top-level `use` reads the imported routine

A routine a unit-level `use` imports is also reached by a `&name` read written
above it (`say &greet4.name; use LateOps3;`), as rakudo does. The fallback no
longer rides an op in front of the call: the compiler only records the later
`use`'s import sets, and the run-time lookup that is about to report the name
as unknown (`Unknown function`, or an unresolved `&name`) consults them last.
A name that already means something else, such as `&ok` taken inside a block
that `use`s Test, keeps that meaning.
