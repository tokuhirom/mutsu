# A block's `use` binds its imported types in the block's type positions

Rakudo installs what a `use` imports in the lexical pad of the block that
holds it. So inside `{ use M; ... }`, a type position such as `sub (Foo $x)`,
`my Foo $y` or `--> Foo` names `M`'s `Foo`. That holds even when the program
declares its own `Foo`, and a routine taken out of the block keeps it. mutsu
resolved these type positions by name at run time against the program's own
`Foo`, so the routine accepted the wrong type and rejected the right one
(ADR-8751 stage 1, imported types).

The parse-time module scan now records each type a module exports, with its
qualified name and export tags (`exported_types.rs`). The type qualification
pass (`parser::type_qualify`) puts the types a block's `use` imports into
that block's lexical scope. The rest of the block's type positions, including
code written before the `use`, then carry the module's qualified name. A
module with a `sub EXPORT` hook or a computed export stash has no such
record, and its types keep the run-time import. The unit's own top-level
`use` is unchanged.
