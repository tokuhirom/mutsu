# A top-level sub reads the unit's lexicals through a frame slot

A sub declared at a program's mainline or in a bare block reads the unit
lexicals it closes over through cells captured into its ADR-0024 bucket. Each
read still resolved the name through the by-name chain before reaching the
bucket: a per-call cache served sigilless scalars from their second read of a
call, and `@`/`%` names not at all.

Such a sub now reads those variables the way a routine-nested sub reads its
aliases: `GetLexFree` through a hidden local of its frame, filled from the
captured cell on the first read of a call. A loop in a top-level sub reading
the unit's `@out` and `$k` went from 6.7k to 5.2k instructions per iteration,
against 4.2k for the same variables declared in the sub.
