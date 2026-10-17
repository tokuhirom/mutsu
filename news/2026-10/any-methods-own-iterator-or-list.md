# Any's list methods read an object's own iterator or list

An object whose class declares `iterator` now answers `head`, `map`, `sort`,
`sum`, `min`, `batch` and the other methods Rakudo writes on
`self.iterator` from its elements. An object whose class declares `list`
also answers `join`, `elems`, `kv`, `flat`, `combinations`, `end` and the
rest of the methods written on `self.list`. Neither class needs to compose
`Iterable` (ADR-11276 §9.148).

Before, mutsu matched Rakudo on 6 of these 18 names for such an `iterator`
and on 12 of the 39 names for such a `list`. Four name lists in the method
dispatch special-cased `Backtrace`, `IterationBuffer`, buffer-backed
`CArray` and user iterators separately; one step now replaces them all.
