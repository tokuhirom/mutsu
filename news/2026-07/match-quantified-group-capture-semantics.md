# Quantified-group capture semantics: DBIish 36-pg-array goes 0 → 46/46

2026-07-29. DBDish::Pg's `PgArrayGrammar`/`_to-array` (the PostgreSQL array
column reader) died on its first row with "Type Array does not support
associative indexing". Unwinding it exposed three general capture-semantics
divergences from raku, all around quantified capture groups
(`( <element> ','?)*`):

1. **A capturing group is a capture boundary.** A named capture inside
   `( $<e>=(\w) )*` belongs to each iteration's own group Match (reached via
   `$0[n]<e>`); raku leaves `$/<e>` entirely absent. mutsu's
   `collect_named_captures_in_atom` descended into `CaptureGroup` atoms when
   collecting quantified names, so the parent Match's hash grew a spurious
   empty-Array entry for every inner name. (A NON-capturing group `[ ... ]*`
   correctly exposes inner names to the parent as lists — that path is
   unchanged and now pinned.)
2. **`Match.values` / `Match.kv` flatten a quantified positional capture.**
   Match is a Capture: `.values` is positional-then-named, and a quantified
   `$0` (an Array of per-iteration Matches) flattens into it — raku's
   `$m.values` over `(\w)*` on "ab" is the two Matches, not a one-element
   list holding an Array. (`.keys` and `.pairs` do not flatten; verified
   against raku and matched.)
3. **The `for <element>.values` writeback desugar mangled non-Array
   elements.** `for %h<k>.values { $_ *= 2 }` compiles to a temp-array copy +
   loop + element write-back (#3156). For a Match element
   (`for $m.<array>.values`) the bare-element copy wrapped the Match into a
   one-element array (iterating the wrong thing) and the unconditional
   write-back then REPLACED the Match with that Array as a silent side
   effect. The copy now goes through `.values` (identical for Positional
   elements) and the write-back is guarded by `~~ Array` — note `~~
   Positional` is not a usable guard, Match does Positional.

Pinned by `t/match-quantified-group-capture-semantics.t` (12 asserts, passes
under raku too). The DBIish upstream Pg suite is now 8/11 files at raku
parity (remaining: 35-pg-common SEGV, 36-pg-enum, 38-pg-errors — see
`todo/tickets/dbiish-pg-upstream-suite-parity.md`).
