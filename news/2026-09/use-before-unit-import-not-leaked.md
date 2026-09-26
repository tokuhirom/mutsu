# A module's own `use` no longer leaks its imports to the importer

In Raku a `use` is lexical to the compunit that says it. mutsu installed a
module's plain-routine imports under the runtime package its body ran in, and
for a file with no `unit` declarator -- or a `use` placed *before* a later
`unit class` -- that package is `GLOBAL`, which the importer shares. So
`use E; ex(1)` reached a routine only `E` had imported, and a second module
declaring its own `sub ex` died with "Redeclaration of routine 'ex'" (the
failure that stopped `use LLM::Agent::Loop; use LLM::Agent::Subagents;`).

After the module body runs, the `GLOBAL::name` aliases its own `use`
statements installed are now moved into the module's compunit-private routine
table, the same table that already holds its private helpers. The module's own
subs and methods still resolve them; the importer does not see them.

The private-helper seclusion also stopped treating a name as public just
because *some other* module exports it: only the loaded module's own exports
stay shared, so a module's private `sub ex` no longer leaks when an unrelated
module happens to export an `ex`.
