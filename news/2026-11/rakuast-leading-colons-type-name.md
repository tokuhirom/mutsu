# RakuAST: `::Array` keeps its leading `::` as `Name::Part::EmptyEdge`

A type term written with a leading `::` (`::Array`, `::Foo::Bar`, `::Int:D`)
was viewed as `Type::Simple.from-identifier("Array")`. The parser drops the
`::` when it looks the type up, so the RakuAST view could not tell it from
`Array`.

Rakudo 2026.11 starts the type's `Name` with a `Name::Part::EmptyEdge`, and
its `.origin` spans the whole `::Array`. A RakuAST parse now records the
spelling (`Spelling::LeadingColons`, kept only when the tree is read as
RakuAST), and the view builds that `Name`. For `::Int:D` the edge sits on the
`Type::Definedness`'s base type. The lookup at run time is unchanged (#18620,
found by the `.origin` differential over `roast/S06-signature/scalar-type.t`,
#15780).
