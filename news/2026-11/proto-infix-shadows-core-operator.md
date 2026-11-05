# An imported `proto sub infix:<op>` shadows the core operator

A module that exports `proto sub infix:<div>(...) {*}` plus `multi` candidates replaces the core
`div` for the importing scope in rakudo. mutsu kept the core `(Int:D, Int:D)` candidate in the set:
the module's own `Int:D` candidate tied it (`Ambiguous call to 'infix:<div>(Int, Int)'`), and a call
none of its candidates accepted (`42.2 div 9`) fell through to the core answer instead of
`X::Multi::NoMatch`. The lexical-import capture now records that the family declared a proto (an
only-star one included), the dispatcher carries a marker, and both the core-tie verdict and the
"no candidate accepts" decline honour it. A `multi` with no proto of its own still joins the core set.
