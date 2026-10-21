# The record of env-key imports is kept per importing unit

An import that still binds through the env (an exported variable, constant or
type the parse-time scan cannot see, or a `sub EXPORT` hook's values) records
the key it wrote. Five readers use that record to tell an imported binding
from a declared one:

- the `MY::` listing;
- the `EVAL` term names;
- the `&name` alias probes of a bare call;
- the module-name visibility gate;
- the own-package type probe of a bareword term.

The record was one process-wide table, `ModuleState::imported_env_aliases`.
A program's `use M` therefore made `M`'s names count as imported in every
other compunit, and a module load took the whole table aside and put it back
so that the module's own imports would not show in the program.

The table is now `ModuleState::unit_env_imports`, in
`runtime::unit_env_imports`. Each key records the units whose code imported
it. A reader asks only about the running code's unit and the units it is
nested in (an `EVAL` unit's parents), so the module load no longer saves and
restores the table. The pseudo-stash spelling is derived from the key
(`import_display_name`). This is item 2 of the #14140 design for ADR-8751's
stage 1 exit.
