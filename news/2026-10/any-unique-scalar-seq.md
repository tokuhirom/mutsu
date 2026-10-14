# Any.unique on a scalar answers a one-element Seq

`1.unique`, `"a".unique` and `<1>.unique` answered the scalar itself. Rakudo's
`Any.unique` is `self.list.unique`, so the result is a one-element `Seq`. The
`unique` method-table handler now answers a scalar receiver (Int, Str, an
allomorph, ...) with a one-element `Seq` and expands a Range into its elements.
Pinned in `t/types/any-unique-scalar.t` (#13235).
