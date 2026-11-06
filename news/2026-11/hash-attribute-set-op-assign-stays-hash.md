# `%!h ∪= $x` leaves a plain hash attribute a Hash

The expression form of a compound assignment to a private/public hash attribute stored the
operator's result as is when it was a Set/Bag/Mix, so `has %!relationships; ... %!relationships
∪= $attr` turned the attribute into a Set. Only an attribute declared `is SetHash`/`is BagHash`/
`is MixHash` keeps the QuantHash now; a plain (or object) hash converts the result like any other
hash variable. Red's `MetamodelX::Red::Relationship` does exactly this, which broke every
`Model.^create` of a model with a relationship (`No such method 'name' for string '@!posts'`).
