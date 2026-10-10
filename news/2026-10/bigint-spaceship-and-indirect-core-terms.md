# Exact BigInt `<=>`/`cmp` and `::("True")` lookups

Found through the Version::Repology suite. `<=>` and `cmp` on Ints beyond 2**53
fell back to an `f64` comparison, so neighbouring big integers compared `Same`; they now
compare exactly. `::("True")`, `::("False")`, `::("Inf")` and `::("NaN")` now
resolve to the core terms instead of returning a "No such symbol" Failure.
All seven Version::Repology test files now pass under mutsu.
