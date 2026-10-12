# A container shared through a cell is locked on the cell and the node

Eight threads that mixed `$c[$k] = $k` with `$c.push` / `$c.shift` on one `:=`-bound array
crashed the interpreter every time. So did a parameter array passed to a routine while other
threads pushed to it. Both now run clean on a debug build: 0 crashes in 6 runs each, against 6
in 6 before (#13016, ADR-0068 §17).

The structure lock of ADR-0068 picked one key per access: the shared cell when the writer went
through one, the container node otherwise. So a `push` and an element store through the same
cell, or a writer through the cell and a reader handed the bare array, held different locks.
A writer through a cell now takes both stripes, in a fixed order. Crashes that come from
refcount corruption in the cell's raw-pointer slot writes are still open on #13016.
