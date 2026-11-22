# A large grammar's first parse no longer re-derives its rule analyses

A grammar's first parse resolves, lowers and analyses each rule it reaches;
later parses reuse all of it. On CSS::Module's CSS3 grammar (a few lines of
CSS) the first parse cost ~1.08G instructions against ~0.24G for every
later one. Four costs in that difference were analysis repeated per query
rather than per grammar (#17640):

- **Proto variants**: finding the `:sym<…>` candidates of a rule scanned every
  registered token key once per distinct prefix asked about. The candidates of
  every prefix are now indexed in one pass per token-definition generation.
- **ASCII sets**: a compiled rule probed its every atom against all printable
  ASCII characters on its first run. Each atom is now probed the first time it
  is actually matched.
- **Subrule argument lists** (`<val(/<x> /, &?ROUTINE.WHY)>`): each was parsed
  twice, once as a possible sequence (`1, 2 ... 10`) and once as ordinary
  arguments. The sequence attempt is skipped when the argument text has no
  sequence operator.
- **Call-edge stability**: whether a rule's call edges can change between
  attempts was re-derived for every analysis that asked; it is now memoized per
  rule and generation.

Together the first parse went from ~1,076M to ~540M instructions; the
steady-state parse is unchanged at ~244M.
