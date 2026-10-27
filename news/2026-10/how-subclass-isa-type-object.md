# `^isa` on a type with a user `Metamodel::ClassHOW` subclass asks about the type

`$type.^isa($other)` reaches a user subclass of `Metamodel::ClassHOW` (Red's `model` metaclass)
as `HOW.isa($type, $other)`. mutsu answered for the HOW instance itself, so
`IndexChild.^isa(IndexRole)` was always false and Red's index-key ownership check rejected a
column inherited from a parent model (`Index keys must reference stored columns of their own
model`). The two-argument metaobject form now asks about the type object it is given.
Red `t/97-indexes` now passes.
