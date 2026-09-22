# Math::NumberTheory dispatch and numeric parity

The ecosystem roulette run for `Math::NumberTheory` 0.1.4 fixed several
interpreter gaps exposed by its test suite: definedness-constrained proto
dispatch, `FatRat`/`BigRat` logarithm, absolute-value, and rounding paths, and
typed object-hash metadata preservation through hyper method calls.

The focused regressions are covered by
`t/routines/dispatch/proto-defined-candidate.t`, `t/types/fatrat-log.t`, and
`t/lang/operators/hyper-object-hash-keys.t`. The distribution now passes 17 of
19 baseline test files; its remaining two files are the existing native
`floor`/`ceiling` return-type issue tracked by [#9012](https://github.com/tokuhirom/mutsu/issues/9012).
