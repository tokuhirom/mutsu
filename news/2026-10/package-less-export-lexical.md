# A package-less module's own `sub ... is export` is lexical to it

A plain `sub f is export` in a module with no `unit` declarator is
`my`-scoped, so it reaches another scope only through an import. mutsu left
it registered as the shared `GLOBAL::f`. The module-load bookkeeping then
treated that key as the module's own permanent registration, so `need M` alone
made `f` callable, and a `{ use M }` block leaked it past its closing brace.

After the module body runs, mutsu now moves the module's own exported `my`
subs into the module's private routine table, as it already did for
unexported helpers. The definition is also kept under `M::f`, where
`import_module` aliases it from. Each import scope now records the
compilation unit that opened it, so an import made inside one of the
module's routine bodies still shadows a same-named routine of that module
(#11103).

What raku merges into the importing scope's GLOBAL (`our sub`, `constant`,
classes) is still published process-wide. That needs unit-aware type
visibility and is tracked in #11136.
