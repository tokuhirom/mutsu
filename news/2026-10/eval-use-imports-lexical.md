# A `use` inside EVAL no longer leaks its imports to later EVALs

An `EVAL` is its own compilation unit, so whatever a `use` inside it imports
is lexical to that EVAL. mutsu ran the snippet without the import scope a
`use`-holding block opens, so imported aliases (a term a `sub EXPORT` hook
returned, an exported class name) stayed in the caller's environment and a
later `EVAL 'U'` still resolved them. `EVAL` now opens that import scope around
its compunit, and the aliases are dropped when it returns; a class the EVAL
itself declares still outlives it, as in Rakudo (#11069).

A package-less module's own exported `sub`/`constant` still stays visible after
any block-scoped `use` of it, EVAL or not; that is tracked in #11103.
