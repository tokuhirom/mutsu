# `is export` routines nested in a routine body are exported

A `my (multi) sub ... is export` declared inside another routine's body was never exported:
mutsu registered it only when the enclosing routine ran, long after the importer's `use` had
copied the module's export table. The importer got `Unknown function`.

Raku runs `is export` at compile time. mutsu now registers (and so exports) every such nested
declaration when the enclosing routine is installed during the module load, from the plan and
compiled body the enclosing routine already carries
([ADR-0132](../../docs/adr/0132-nested-routine-exports-install-at-enclosing-routine-registration.md)).
Called during the enclosing routine's dynamic extent, the exported routine reads its free
variables from that routine's live frame, as in Rakudo.

This is the shape of the `Green` distribution's `set`/`test` pair (`t/01-time.t`); mutsu#10050.
