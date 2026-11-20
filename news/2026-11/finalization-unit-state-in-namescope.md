# The finalization's unit state lives in the NameScope

Lowering a setting term (`pi`, `True`, `Nil`), a routine word or a user infix
asks what the unit declares: the constants and terms that shadow a setting
name, its sigilless terms, its packages, its routines, and, per statement list,
the `True`/`False` it redeclares and the `infix:<…>` it declares. Those answers
lived in thread-locals (`shadowed_terms`, `declared_routines`, `infix_func`)
that `finalize_unit` and `EVAL($ast)`'s `lower` reset and overwrote for each
unit without restoring them. A finalization running inside another clobbered
it, and a reader outside any unit (the `$*CU` annotation, a node built at CHECK
time) saw whichever unit happened to run last.

They are now fields of the `NameScope` the unit is finalized in (ADR-15760
§2.5 item 4): `UnitGuard` installs a unit's facts with fresh per-list stacks
and restores the enclosing unit's on drop, and `ListScope` opens one statement
list's scope. The `$*CU` annotation scans the tree it annotates; a CHECK-time
node hoists no routine.

This is slice 2a of ADR-15760 Amendment 2, the precondition for finalizing
each statement list when the parser closes it: the audit recorded in ADR §7.5
finds the finalization otherwise pure (guard-scoped state, idempotent parser
registrations, counters that only mint fresh names), and
`src/rakuast/finalize_purity_tests.rs` pins it (#15760).
