# A module that re-exports a name keeps calling the one it imported

JSON::Pretty exports `&to-json` as its own multi dispatcher while its candidates call the
`to-json` they imported from JSON::Fast. mutsu keeps a single `&name` slot in `env`, so the
second import replaced the first and the module's own call recursed into itself until the stack
ran out (`t/04-roundtrip.rakutest` died with 0 of 18 assertions; it now passes 18/18).

Three changes fix it:

- A multi dispatcher built for `&name` now carries its candidates' source file, so the "declared
  in the calling unit" check that already protected plain subs covers multi/proto exports too.
- The `sub EXPORT`-hook call path applies the same check (it had none for names without a plain
  package routine, which is every `proto` name).
- The interpreter records which `&name` callable each compunit imported through an `EXPORT` map
  (`unit_imported_callables`), so a call rejected as "the override is from my own unit" resolves to
  what that unit imported instead of falling off the end.

Pinned by `t/modules/import-export/export-hook-reexport-over-own-import.t`. The ledger record is
not re-measured here (no rakudo in this container); run `ecosystem-sweep.yml` scope=only for
JSON::Pretty after merge.
