# A bound array's element store stays fast once threads exist

`@a[$i] = $v` has a lane that writes the slot directly, but it stood down
for the rest of the process the moment any thread was spawned: a plain
`@` lexical a worker captured is then routed by the name-keyed cross-thread
lanes, whose semantics the lane does not reproduce. Every store, on every
thread, then paid the full path, about 30,000 instructions per store
against the lane's ~1,100.

A `:=`-bound array is not one of those. Every alias reaches it through one
shared `ContainerCell`, the name-keyed lanes already decline for it, and the
full path's store is an in-place write of the cell's node under ADR-0068's
container lock. So the lane now serves a celled array on every thread,
under that same lock: it re-checks under the lock that the cell still holds
the node it checked and that the slot still takes a plain store, and
otherwise declines to the full path. The store opcode already holds the
variable's guard, so the lane asks whether the held guard covers the cell
and the node (`ContainerStructGuard::held_covers`) instead of taking a
second one.

`bench-threads`' write section (four workers storing into disjoint slots of
one bound array) went from ~1,450 ms to ~260 ms on a 4-core container; one
worker alone from 97 ms to 21 ms (#8069).
