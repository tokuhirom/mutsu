# A role body's sub keeps its composition parameters after a routine exits

A role body's `sub` that reads a role parameter (`role BE[:$prefix] { sub lookup($s is copy) { "$prefix$s" } }`)
lost the parameter on the second construction of the same specialization when the first one was composed inside a
routine that declares inner routines (a `LEAVE`/`UNDO` phaser is enough). The routine's exit restored the routine
registry and dropped the class-package copy of the sub, while the composed class stayed cached. The re-declared subs
are now kept like an `our` sub of the class's package. Marrow `t/02-db.t` (DB::SQLite opens a `BitEnum[...]` per
connection) no longer sees an uninitialized `$prefix` on the second connection (#16899).
