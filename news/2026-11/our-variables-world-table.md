# `our` variables and `our sub`s are shared by every thread

An `our` variable is package state: rakudo keeps one stash per package per
process. mutsu gave every spawned thread a deep copy of its `our` store, so
an `our` that a worker declared (`await start { EVAL q[our $x = 42] }`), a
package variable it first wrote (`$Q::fresh = ...`) or a rebind it made
through the stash (`Q::<@list> := [...]`) never reached the main thread.

ADR-17216 slice 4b makes the store one world table (`OurVarTable` in a
`WorldCell`), shared by every thread: reads take the thread's snapshot,
writes publish under the table's lock. The variables are sharded, so a write
while other threads hold snapshots copies one shard. No reference into the
table outlives its lock: an in-place mutation (`@list.push(...)` from the
package's own sub) pins the stored container in a per-thread slot and writes
through it, and the identity-preserving mirror store is one table write.

An `our sub` is published to the same table, so one declared at run time on
a worker (`EVAL q[package PW { our sub get {...} }]`) is found on the main
thread by `PW::get()`, `&PW::get` and `&::("PW::get")`. Two single-thread
gaps closed on the way: an indirect `&::("name")` now also looks in `OUR::`,
as raku does (an `our sub` an `EVAL` declared was "No such symbol"), and a
package-qualified array read (`@Pkg::a`) reads the package store first, as
hash and scalar reads already did, so a rebind made in a sub is seen by its
caller.
