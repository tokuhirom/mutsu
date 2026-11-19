# A closure run on another thread resolves routines where it was written

Every thread keeps its own copy of the routine tables, taken when it is
spawned (ADR-17216 §3.2). A closure written on one thread and run on another
used to resolve its by-name calls through the copy of the thread running it.
A worker started early, at `BEGIN` or by a module's mainline during `use`, and
fed closures through a `Channel` never re-forks. So a routine declared after
the worker started was `Unknown function`. A multi that gained a candidate
later also dispatched over the stale candidate set: `m(Str)` was
`Cannot resolve caller` although `multi m(Str)` was declared (#18135).

A closure now records the routine tables of the execution context it was
created in. When another context runs it, and that context's tables differ
from the creator's current ones, the call runs with a copy-on-write share of
the creator's tables installed, and the caller's tables come back afterwards.
A declaration made during such a call stays in the share. So a routine scope
on one thread still cannot install its inner subs where another thread
resolves them.

This is the routine half of Green's failure (#18090): its tests' `ok` now
resolves on the worker the module starts. The module-statics half is #18136.
