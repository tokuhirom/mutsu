# Package and class-body statics are shared by every thread

A `package P { my @log; ... }` block lexical and a class-body `my` static
(`class C { my $n; method bump { $n++ } }`) live in one table per program,
`package_lexicals`. Until now every spawned thread got a copy-on-write
snapshot of it, so a static that a worker created (a class `EVAL`'d inside a
`start`) was invisible to the main thread (`Nil`), and a static the worker
rebound diverged from the main thread's copy.

ADR-17216 slice 4a makes the table, and its two companion tables (the names
of class-body statics and their declared types), world state: one
`WorldCell` shared by every thread. Reads take the thread's snapshot; writes
run under the table's lock and publish to every thread. No reference into
the table outlives that lock any more: the in-place mutation of a
package-block `@`/`%` lexical (`@log.push(...)` from one of the block's
routines) used to take a `&mut` straight into the table entry; the entry is
now boxed into a cell under the write lock, and the mutating frame holds the
cell while it writes through it, so another thread's rebind cannot free it
mid-mutation. TRIR's free-variable cache is keyed on the table's world
generation too, so a binding another thread adds invalidates it.
