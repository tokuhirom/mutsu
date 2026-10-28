# A routine with CATCH and LEAVE/KEEP/UNDO/ENTER returns its last value

A body holding both a `CATCH` (or `CONTROL`) and an ENTER/LEAVE/KEEP/UNDO phaser
compiled its phaser block in statement position, which discards the tail value,
and then read the enclosing `$_` instead -- so the routine returned `Any`. The
try region now compiles that body as a value-position phaser scope. Red's
`automatic-transaction` (`CATCH`, `KEEP`, `UNDO`, then `block()`) lost every
transaction block's result, failing the nested-transaction assertions of
t/98-savepoints and t/99-transaction-guard.
