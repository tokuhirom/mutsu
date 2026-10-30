use Test;

plan 8;

# A Rakudo metarole (Metamodel::Naming, ...) is an NQP-level object, "not
# actually Mu": Metamodel::Primitives.is_type is true only for the metarole
# itself. The HOW classes and Primitives are ordinary types.
is Metamodel::Primitives.is_type(Metamodel::Naming, Mu), False, 'a metarole is not a Mu';
is Metamodel::Primitives.is_type(Metamodel::Naming, Any), False, 'a metarole is not an Any';
is Metamodel::Primitives.is_type(Metamodel::Naming, Metamodel::Naming), True, 'a metarole is its own type';
is Metamodel::Primitives.is_type(Metamodel::Stashing, Metamodel::Naming), False, 'a metarole is not another metarole';
is Metamodel::Primitives.is_type(Metamodel::Archetypes, Mu), False, 'Archetypes is NQP-level too';
is Metamodel::Primitives.is_type(Metamodel::ClassHOW, Mu), True, 'a HOW class is a Mu';
is Metamodel::Primitives.is_type(Metamodel::ClassHOW, Metamodel::Naming), True, 'a HOW class does its metaroles';
is Metamodel::Primitives.is_type(Int, Mu), True, 'an ordinary type is a Mu';
