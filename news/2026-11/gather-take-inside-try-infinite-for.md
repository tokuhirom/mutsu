# `take` inside a `try` body no longer hangs a lazy gather over an infinite `for`

`gather for 1..* -> $n { try { take $n } }` pulled one element and then ran forever. The `try`
body is pushed as a block callframe (so backtraces show it), and the lazy-pull depth check counted
that frame as a call nested under the pull driver; the take-limit suspension was therefore
deferred to a loop boundary that a `for` never consumes. `lazy_pull_routine_depth` now skips
blocks without a declaring file (the `try` body and a bare block with `CATCH`), the same idiom
`unit_scoped_sub_index` uses, so the take suspends in place and the existing `try` resume state
continues it. Found through Math::Sequences' `t/numberphile.rakutest` (A002904, "Roman").
