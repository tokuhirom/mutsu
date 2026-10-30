# `Metamodel::Primitives.is_type` treats a metarole as an NQP-level object

`Metamodel::Primitives.is_type(Metamodel::Naming, Mu)` answered True; a Rakudo
metarole (and `Metamodel::Archetypes`) is an NQP-level object, "not actually
Mu", so it is a type only of itself. HOW classes and `Primitives` stay ordinary
types. `Kind`'s `Refine.ACCEPTS` branches on this to pick its `HOW`-based check
for the Rakudo metaroles.
