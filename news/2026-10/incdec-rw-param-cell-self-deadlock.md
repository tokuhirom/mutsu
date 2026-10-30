# `$pos++` on an `is rw` parameter no longer deadlocks on its own cell

`atomic_container_incdec` held the parameter's cell lock while it asked the env for the variable's
type constraint, and that lookup compares bindings through their cells, locking the same
non-reentrant mutex again. A `CArray[num64]` element store earlier in the program was enough to
leave the env holding that cell, so MongoDB::Fast's `BSON.decode` (three `$pos is rw` methods deep)
hung. The cell is read, the lock released, and only then is the type looked up.
