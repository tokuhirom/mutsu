# An unset parameterized attribute holds the real type object

`has CArray[uint16] $.k` with no value seeded the attribute with a package
named `CArray[uint16]`, not the type object `CArray`'s `^parameterize` builds
(a mixin of the typed-array role). `W.new.k ~~ CArray[uint16]` was therefore
False, and its `.^mro` lacked `CArray` (#14441). The seed now asks the same
meta-parameterization a `my CArray[uint16] $x` declaration uses.
