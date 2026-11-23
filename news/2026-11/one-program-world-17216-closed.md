# Every thread now sees one program world (#17216 closed)

#17216 reported that a spawned thread ran on a snapshot of the program's
world. A class `EVAL`'d on one thread, a routine wrapped there, or a handle
opened there was not seen by another thread. ADR-17216 replaced that snapshot
with world tables that every thread shares. Its last three PRs did the
following:

- They made `our` subs and packages declared on a worker visible through
  every stash spelling.
- A module now loads once when several threads `require` it at the same
  moment.
- A grammar's rules now reach every thread.

The issue's repro now prints what rakudo prints, and a spawn no longer forks
the type registry or the handle table. Twenty more cross-thread shapes,
checked against rakudo, also match. ADR-17216 §3.8 lists the per-thread state
that a spawn still copies, and why.
