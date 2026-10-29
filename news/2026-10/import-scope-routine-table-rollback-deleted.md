# A block's import no longer snapshots and rolls back the routine tables

`pop_import_scope` used to snapshot the function and proto tables when a
`use`-containing block opened and take back every key the block's imports
installed when it closed (plus restore the enclosing family an imported proto
had hidden). What a block imports binds in its lexical import sets, a block's own
`sub` is scoped by `BlockScope` / `RoutineScope`, and measured over all of `t/` and
the whitelisted roast files the rollback changed nothing except for `use Test`
(`Test` and `Test::*` had their own compile arm that bypassed the import set,
so `proto sub skip` stayed a registry alias after the block). That arm now opens
the block's set like any module's `use`, and the rollback is gone: the scope
exit keeps only the foreign-`MAIN` drop (a `MAIN` a block imported is not the
program's).

Deleted: the `ImportScopeSnapshot` registry-table fields and shadowed-family
bookkeeping, `import_scope_keeps_function`, `log_import_scope_function_write`,
`Registry::function_writes`, and the in-block branch of
`shadow_imported_proto_family` (a top-level import still replaces a visible
family).
