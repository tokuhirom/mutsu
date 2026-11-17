# A class's own `^parameterize` runs at compile time

A type written `C[T]`, for a class `C` with its own `method ^parameterize`,
is now built when the code is compiled, as Rakudo does (#17085): the
meta-method runs before the unit's run-time statements, in source order with
its `BEGIN` blocks, and a loop body's `my C[T] $y` no longer calls it on each
iteration.

Each top-level statement that spells such a parameterization -- a variable's
or parameter's type constraint, a return type, the term `C[T]` -- gets a
`BEGIN C[T]` ahead of it in the unit's BEGIN prologue
(`begin_prologue::parameterizations`), and a parameterization by type objects
is built once per (class, argument types), so the run-time uses find the type
already made. An argument the code binds at run time (`sub mk(\t) { C[t] }`)
still parameterizes when the code runs.

Rakudo calls the meta-method once per spelling in the source; mutsu builds one
type per argument types, so a program that spells `C[Int]` twice sees one
call. Only classes the unit itself declares are lifted; a class imported from
a module is still parameterized when the code first needs it.
