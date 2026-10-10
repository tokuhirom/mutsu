# A bare block's implicit topic no longer itemizes a list

`{ Pair.new('E', $_) }.(("a", "b"))` and `{ (1, $_) }` boxed the implicit topic into a
Scalar cell, so `.raku` printed `:E($("a", "b"))` where Rakudo prints `:E(("a", "b"))`.
The topic bound to a bare List/Array/Hash/Seq now stays the plain value when captured as a
call argument or list element; a real `$`-container argument keeps its `$(...)`. This was
the cause of the extra container in FunctionalParsers' EBNF parse results (#12518).
