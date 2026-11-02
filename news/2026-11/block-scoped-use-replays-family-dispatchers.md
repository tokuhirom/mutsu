# A block-scoped `use` of a multi-operator module replays after its first run

A `use` inside a routine body runs on every call of the routine. For a module
that exports plain routines alone, later runs already replayed the first one
(#13906); a module that exports multi operator families (FiniteField's
`infix:<*>`/`<+>`/`<->`) re-imported in full each time, building a dispatcher
`Sub` and a captured env per candidate and dropping the previous set: about
0.5M instructions and ~100 us per executed `use`, against nothing in rakudo
(the import is compile time there). The replay now also covers sets whose
bindings are family dispatchers, unless a `sub EXPORT` hook contributed to the
set (a hook may return different code per run).

EC `t/secp256k1.t` uses FiniteField in `TWEAK`, `x`, `y`, `double` and `add`:
`method b { use FiniteField; 1 }` went from ~94 us to ~1.3 us per call, a
trivial `5 / 1` under it from ~230 to ~56 us (#15981).
