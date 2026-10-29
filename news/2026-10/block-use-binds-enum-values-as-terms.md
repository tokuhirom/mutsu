# A block's `use` binds an exported enum's values as pad terms

The parse-time export scan now lists the values of an `is export` enum as term
variables, so a `use` below the unit's top level declares them as bindings of
its block (ADR-8751, #14140), the way it already does for exported constants
and types. The import writes no `__mutsu_enum_bare_*` env alias for them, so
the import scope has nothing to roll back for an enum: a routine written outside
the block, `EVAL` and `::()` no longer see the values, and a closure taken in
the block keeps them. The binding reads a module's top-level enum key from the
module's own table after its load has finished.

Along the way, a module routine's bare name now resolves to the module's own type
before a term its caller bound under the same name (an exported `constant Error`
beat the module's `class Error` before).
