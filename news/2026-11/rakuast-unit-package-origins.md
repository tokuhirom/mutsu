# Ranges of `unit module` and `unit package`

`unit module Foo;` and `unit package P;` hold the rest of the unit as their body.
Their statement and declaration now run to the end of the unit's last statement
(the `;` terminators after it included), and the name and the body (block,
blockoid, statement list) have their own ranges, as in rakudo 2026.09 (#15780):
`unit module Foo;\nsub f { 1 }\n` is 0-28 with the name at 12-15 and the body at
17-28, where mutsu answered 0-15 and no name or body range. The parser records
the name's position for the unit form; the conversion, which already moves the
rest of the unit into the declaration, adds the body's. `unit class`, `role` and
`grammar` are not covered yet.
