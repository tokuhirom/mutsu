# A type name no term binds skips the term stores

Every hit of the type-check memo asks whether a constant or sigilless term of
the checked type's name (`Bool`, `Str`, ...) shadows the type here. Past the
frame `env` and the unit lexicals, that walked the module-scope tables, the
enclosing package chain and the `our` store, and for a core type name every
one of them missed. Those stores already keep name filters (the module-scope
and package-lexical tables' key unions, the `our` table's index of unqualified
parts), so `term_binding_sym` now asks the filters first and skips the walk
when none can hold the term key. A debug assertion checks that the walk would
have missed too.

The AttrX::Mooish t/090 probe drops by a further ~2.5% Ir per round (#18274).
