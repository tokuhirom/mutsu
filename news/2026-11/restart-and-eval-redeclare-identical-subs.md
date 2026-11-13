# An identical sub in an EVAL or a CHECK-rewritten unit is a routine of its own

A `sub` declared in an `EVAL`, textually identical to one the program already
declared, was taken for that declaration and kept: calls to it ran the
program's routine, which read and wrote the program's lexicals instead of the
EVAL's. The same happened to every routine of a unit a CHECK phaser rewrote:
the unit restarts from the rewritten tree, and its routines were identical to
the first compile's, which closed over a mainline that never ran. A routine
called through a `map` or a method then wrote a copy of the mainline's
variable the mainline never saw (Red's t/124: `The captured argument ran once
during query construction`).

Registration now treats a declaration from another source file, or from a
restarted unit, as a new routine.
