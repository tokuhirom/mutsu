# `.^rw` / `.^set_rw` on ClassHOW

`Metamodel::AttributeContainer`'s `rw` and `set_rw` metamethods are now
implemented. `class C is rw { ... }` records the flag on the metaclass, so
`C.^rw` answers `1` (and `0` for an ordinary class, a core type or a fresh
`Metamodel::ClassHOW.new_type`), and `.^set_rw` sets it. A user subclass of
`Metamodel::ClassHOW` inherits both, and a role's metaclass still has no
`.^rw`, matching Rakudo. This unblocks `Red`'s `MetamodelX::Red::Model.add-column`,
which tests `type.^rw` for every column (#9542).
