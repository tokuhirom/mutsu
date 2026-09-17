# Custom `sub EXPORT` symbols are now visible to MY::/LEXICAL

A symbol a module's custom `sub EXPORT` installed (the selective/renaming
import idiom `sub EXPORT(*@names) { Map.new(...) }`) resolved fine by
bareword or `&`-sigil lookup, but was invisible to `MY::<&name>:exists` and
`MY::.keys` — the whole `MY::` stash could come back empty right after such a
`use`, even though the symbol worked when called directly.

`MY::`/`LEXICAL::` compile to a static snapshot of the compiler's own lexical
scope (`Compiler::emit_lexical_stash`) plus the `imported_env_aliases` table
that `import_module` (the tag-based `is export` path) populates via
`record_import_env_key` — not a dynamic scan of the runtime environment.
`install_export_symbol` (the custom `sub EXPORT` path) never called
`record_import_env_key`, so its symbols never reached that alias table.

Fixing that surfaced a second, more subtle bug: a module's BEGIN-time
preload (`push_preload_scope`/`pop_import_scope`, used when a `use` sits
inside a nested block) already discards the bare env aliases it wrote once
preloading finishes, on the assumption that the "real" import at the
in-position `use` will reinstall them. That is fine for a tag-based `is
export` routine, which also gets a registry entry that survives the
discard — but a custom `sub EXPORT`'s installed symbol lives *only* in `env`.
Once `record_import_env_key` made that alias poppable, a `sub` hoisted to the
head of the *same* package block (mutsu hoists every `SubDecl` so forward
references resolve, emitting its `RegisterDecl` before the block's own
in-position `use` runs) could permanently lose the symbol: JSON::Fast's
`use JSON::Fast; sub render(...) is export { ...to-json(...)... }` shape
(`t/routines/call/tail-stmt-call-named-value.t`) started failing with
"Unknown function: to-json".

The preload's scope-pop now keeps bare env aliases exactly like it already
keeps the classes and `::`-qualified definitions a preload registers — a
real scope-exit removal still happens for a genuine user block, which pairs
its own ordinary (non-preload) `PushImportScope`/`PopImportScope` around the
in-position `use`.

Fixes [#8564](https://github.com/tokuhirom/mutsu/issues/8564).
