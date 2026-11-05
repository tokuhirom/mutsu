# `Metamodel::SubsetHOW.new_type(:$refinee)` keeps the base type

A base type held in a variable (`:refinee($refinee)`) arrives wrapped in its Scalar, and
the subset it minted was registered with the Scalar's string (`(Numeric)`) as its base,
so `.^refinee` was not the type object. The value is now decontainerized first, as the
predicate already was. Kind::Subset::Parametric `t/01-metamodel.t` goes from 7 to raku's 8 ok.
