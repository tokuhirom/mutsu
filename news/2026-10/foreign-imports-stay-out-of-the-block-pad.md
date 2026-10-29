# A module loaded in a block no longer records its own imports on the block's scope

`record_import_env_key` and `record_imported_routine_alias` now share
`importing_scope_mut`: an import is recorded on the innermost open import scope
only when that scope belongs to the importing compunit. A module loaded while a
block's scope is open imports NativeCall's types and constants for itself, and
they no longer enter the block's `imported_env_keys` (one more step toward
retiring the env-key rollback of ADR-8751).
