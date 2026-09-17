# Math::SpecialFunctions goes green with dispatch and lazy-sequence fixes

`Math::SpecialFunctions` 0.1.1 now passes all four of its test files under mutsu,
matching Rakudo's 13 baseline assertions. The fixes cover three interpreter gaps:

- Multi dispatch no longer lets `is copy` outrank an otherwise equal candidate,
  while retaining the dispatch significance of `is rw` and `is raw` for writable
  arguments.
- Integral `Rat` and `FatRat` positional indices keep lazy sequence reads bounded
  instead of forcing an infinite source.
- A slipped lazy map inside a list-context zip is flattened to the finite prefix
  the zip consumes, enabling the module's gamma-function reduction.

The focused regressions are recorded in `t/routines/dispatch/`,
`t/collections/lazy-seq/`, and `t/collections/transform/zip-operator.t`.
