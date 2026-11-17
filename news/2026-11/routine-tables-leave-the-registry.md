# The routine tables leave the registry

ADR-17216 (one program world, shared by every thread) shares the type
registry between threads. Starting that slice showed that `Registry` held two
kinds of state: the package-scoped declarations (classes, roles, enums,
subsets, method tables), which are world state, and the routine tables
(`functions`, `proto_functions`, `proto_subs`, `our_scoped_functions`,
`token_defs`, `proto_tokens` and their memos), which mutsu uses as a
lexical-scope emulator: a routine scope or `EVAL` snapshots them and puts the
snapshot back when it ends. Shared between threads, those restores would leak
one thread's inner `my sub` to a concurrent call of the same routine on another
thread and drop declarations made meanwhile.

So the routine tables moved into their own `RoutineTables`, reached through
`routines()` / `routines_mut()` and forked copy-on-write per spawn, as the whole
registry was. The split is a refactor with no change of behaviour, and it has a
side benefit: a routine-table write no longer goes through the registry's write
guard, which dropped the lexical type-name indexes on every such write; a
routine that declares an inner `my sub` now copies no program table when it
runs on a fresh thread (the `my enum` copy-on-write pin went from 2 copies to
0). ADR-17216 §3.2 records the refinement; sharing the type registry itself is
the next step.
