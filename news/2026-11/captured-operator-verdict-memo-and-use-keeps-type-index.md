# Block-scoped operator imports: memoized core verdict, and `use` keeps the type-name index

Two costs of running FiniteField's `*`/`+`/`-`/`==` through a block-scoped `use`:

- A captured (imported) candidate of a built-in operator is ranked against the
  operator's core candidates on every call. The registry path memoized that
  verdict per `(operator, candidate, operand types)`; the captured path did not
  (`core_infix_verdict`, ~42K of a ~115K-instruction field `*`). It now shares
  the memo (`core_infix_captured_verdict`): field `*` 20 -> 11-13 us.
- Writes to the routine tables (`functions`, `proto_functions`, `proto_subs`)
  no longer drop the registry's lexical-type-name index. A `use` writes
  hundreds of them, and the next bare type-name lookup rebuilt the index from
  every class/role/enum/subset key (~120K instructions per executed `use`).

EC `t/secp256k1.t`: 32 s -> 29.6 s on top of #16018's 92 -> 32 s (#15981).
