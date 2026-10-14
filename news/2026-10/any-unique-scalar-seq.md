# Any.unique on a scalar answers a one-element Seq

`1.unique`, `"a".unique` and `<1>.unique` answered the scalar itself. Rakudo's
`Any.unique` is `self.list.unique`, so the result is a one-element `Seq`. The
`unique` method-table handler now declines a non-list scalar receiver and the
shared `dispatch_unique` reads it as its one-element list, honouring `:as` and
`:with` too. Pinned in `t/types/any-unique-scalar.t` (#13235).
