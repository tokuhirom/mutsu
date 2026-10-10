# A punned role inherits the class named by its `is` header

`role Pun is P { }` (P a class) now gives the pun `P` as its parent: `Pun.new.p`
finds `P`'s methods and attributes and `Pun.new.^mro` is `Pun P Any Mu`, as in
Rakudo. The pun class built by `ensure_role_punned_to_class` carries the parents,
and the registry resolves the MRO of a withdrawn role pun through its class parents
so the instance keeps its lineage after construction (#11623).
