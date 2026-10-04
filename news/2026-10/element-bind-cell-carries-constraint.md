# A `:=`-bound element keeps its source variable's type and default

`my Int $y = 1; %h<k> := $y` promotes `$y` into a shared cell that the hash
element and the variable both hold. That cell did not carry `$y`'s declared
`of` constraint or `is default`, so a `Nil` stored through a sigilless alias of
the element (`for %h.values -> \w { w = Nil }`) reset `$y` to `Any` instead of
`Int` (or its default).

Every site that promotes a `:=` element-bind source now goes through one helper,
`promote_bind_source_cell`, which registers the variable's constraint and
default on the new cell, the way list aliasing already did. Closes #11618;
the element-assignment side of the same gap is #11810.
