# RakuAST: the Name of a bare type or term has an origin, and Term::Enum visits it

`Int`, `RakuAST::Class` and the like are a `Type::Simple` holding a `Name`, and rakudo gives that
`Name` the same range as the type. mutsu gave the type an origin but left the `Name` without one, so a
walk that does `with $node.origin { ... }` skipped it (the largest class in the #15780 differential).
The range is set only when the source text at the term's range is exactly the name, so a
parameterized type (`Array[Int]`) is left as it was.

`Term::Enum` (`True`, `False`) also now visits its `Name`, as rakudo's does; mutsu's
`visit-children` returned nothing for it. In a 12-file sample of the differential the missing `Name`
origins went from 50 to 0. The rest of #15780 (`ArgList`, `IntLiteral` in a parenthesized
subscript, `MetaMethod`, `SemiList`, ...) stays open.
