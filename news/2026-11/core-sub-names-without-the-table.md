# Declaring a `sub` no longer parses Rakudo's whole candidate snapshot

A program's routine declaration asks whether its name is a core routine's
(`note_main_unit_routine`, #17446): a main-program `sub item` hides the core
`item` from module code. That question went through `core_sub_candidates`,
whose first call parsed all of `rakudo_candidates.txt` into one table: 600 KB,
every sub and method candidate, in SipHash maps. That came to about 14M
instructions, five times the rest of the startup of `sub f() { 1 }; say 1`.
The cheap "is this the main program" test also ran after the probe, so module
routines paid for it as well (#17504).

- The lineage test now comes first, so a module's routine never reaches a
  name probe.
- `is_core_sub_name` answers from a set of the core sub names alone, scanned
  from the snapshot's sub rows.
- The candidate tables are split. Subs and methods are each parsed on their
  first use, from their own stretch of the snapshot. The generator writes every
  sub row before the first method row, and a test pins that order.

`sub f() { 1 }; say 1`: 16.9M → 4.6M instructions, 6.8 ms → 4.5 ms wall
(median of 40 runs).

`scripts/bench-det.sh`, release build, `main` `b9cb2131` vs this change:

| benchmark | instructions | allocations |
|---|---|---|
| bench-regex-long-subject | 78.4M → 66.1M (-15.7%) | 41.1k → 37.8k |
| bench-fib | 3,469.0M → 3,445.8M | 7.6k → 4.3k (-44%) |
| bench-regex-capture | 420.8M → 409.7M (-2.6%) | 374.9k → 371.6k |
| bench-startup | 2.67M → 2.68M | unchanged |
