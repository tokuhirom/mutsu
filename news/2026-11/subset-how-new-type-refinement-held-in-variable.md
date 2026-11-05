# `Metamodel::SubsetHOW.new_type(:refinement($pred))` calls a predicate held in a variable

A refinement passed as `:refinement($pred)` arrived in its Scalar and the minted subset never called
it (`A ~~ $subset` was False without running the predicate), while the same block written inline
worked. Kind builds its subsets this way, so every subtest of its `t/01-typecheck.t` failed. The
refinement is decontainerized when the subset is minted; Kind 1.0.3's `t/01-typecheck.t` (10) and
`t/02-meta.t` (3) now match rakudo. Refs #15349.
