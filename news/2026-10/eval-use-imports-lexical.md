# A `use` inside EVAL no longer leaks its imports to the caller

An `EVAL` is its own compilation unit, so whatever a `use` inside it imports
is lexical to that EVAL. mutsu ran the snippet without the import scope a
`use`-holding block opens. So the aliases `import_module` writes into the
environment stayed behind: a later `EVAL 'U'` still resolved a term a
`sub EXPORT` hook had returned, and an exported `$var` overwrote the caller's
same-named variable. `EVAL` now records those aliases in an import scope
around its compunit and drops them when it returns. Its routine and class
registries keep their existing EVAL rollback, so an EVAL's own `our sub` and
the classes it declares still outlive it, as in Rakudo (#11069).

A loaded module's own top-level `sub`, `constant` and exported class still
stay reachable after any block-scoped `use` of a package-less module, EVAL or
not. That is tracked in #11103.
