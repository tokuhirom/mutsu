# A block's `use` no longer copies the registry's key sets on entry

A block, routine body or `EVAL` that holds a `use` runs inside an import
scope, so the routines and classes it imports roll back when it exits.
Opening the scope collected the key set of the routine table, the class table
and both proto tables into fresh hash sets, every time the block ran. Closing
it then rebuilt the set of classes to keep by walking every registered class
(#9170).

All four tables are copy-on-write, so the scope now holds the tables
themselves, which is a reference count bump each. The close compares each
current table with the one the scope opened on. A table the body never wrote
is skipped. A program-declared `GLOBAL::MAIN` is kept as before. The routine
table's rollback filter and the class table's keep set now run only for a
table the body did write.

`for ^200 { use Test; ... }` went from 4.78M to 3.06M instructions per
iteration (release, JIT off). The close dropped from 1.55M to 0.24M, and the
open no longer shows in the profile. What remains is the re-import itself
(`import_module`, 1.3M).
