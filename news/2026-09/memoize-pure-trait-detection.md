# Memoize now respects `:pure(False)` on callable traits

Callable `.can` now returns a proper method list, including an empty list for a
missing method. This matches Rakudo's list semantics and lets Memoize distinguish
an explicitly impure routine from one that is implicitly pure.
