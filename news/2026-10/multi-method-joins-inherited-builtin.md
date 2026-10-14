# A user `multi method` that rejects a call falls through to the inherited built-in

In Rakudo the `multi method` candidates of a class (and an `augment`'s) join the candidates the receiver
inherits from a built-in proto, so `class D { multi method elems(Int $x) {...} }; D.new.elems` is `1`, and
`augment class Any { multi method map(Int $x) {...} }` leaves `(1, 2).map(* + 1)` working. mutsu raised
`X::Multi::NoMatch` ("Cannot resolve caller map(List:D: WhateverCode:D)") whenever no user candidate bound.

`instance_method_not_found` now runs the built-in method of that name (with the user override hidden from the
gates, as for a `callsame` out of an augmentation) when the receiver is not a type object and its MRO has one
and no explicit `proto` owns the dispatch; a name with no built-in is still `X::Multi::NoMatch`. The row table
has no signatures, so "the built-in is a multi" is approximated by "the MRO has a built-in of that name".

Still open on #12751: ranking a user candidate *against* the built-in's (`augment class Any { multi method
first(Callable $c) }` still beats `List.first`, whose `Any:D:` invocant is narrower in Rakudo) needs per-row
candidate signatures. Pinned by `t/oo/method/multi-method-joins-inherited-builtin.t`.
