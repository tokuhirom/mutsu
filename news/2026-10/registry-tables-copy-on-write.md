# The builtin registry is no longer copied whole on the first write

Every interpreter starts from a shared builtin registry, and the first
registry write of a process copied all of it. Loading `use Test` triggered
that copy as soon as it registered a routine. The large tables now sit behind
a `CowTable`, which shares a table copy-on-write:

- the class, method-entry and role/class relation tables;
- each class definition inside the class table.

The registry copy is now a reference-count bump per table. A table, or one
class definition, is copied only when it is itself written. A role declaration
also no longer removes a name it never registered: through a shared table,
even that removal would have copied the table.

Measured on `use Test; ok 1;` minus an empty script (profiling build, warm
cache, callgrind), the load costs 1.2M instructions less (ADR-12026 §2.3).
The price is about 0.3M more at startup: each of the 367 builtin class
definitions is allocated behind its own reference count once per process.
