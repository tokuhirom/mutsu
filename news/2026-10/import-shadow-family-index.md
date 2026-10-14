# A block's import of a proto or only sub reads its family from the index

When a block imports an `only` sub or a proto'd routine, the import hides the
enclosing family of the same name, and the scope's exit puts that family
back. To find the family's keys, `shadow_imported_proto_family` scanned every
key of the import scope's routine-table snapshot, and then every key of the
current table. It did this once per such export. A `use Test` inside a loop
paid those scans ten times per iteration (#9170).

The snapshot is now the routine table itself, so the family is read through
that table's family index (`FunctionTable::family_keys`). That costs the
family's own names, not the whole table.

`for ^200 { use Test; ... }` went from 3.06M to 2.46M instructions per
iteration (release, JIT off). The two import-scope changes together took it
down from 4.78M.
