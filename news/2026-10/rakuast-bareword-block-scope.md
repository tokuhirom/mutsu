# RakuAST: barewords resolve against the innermost block's declarations

`.AST` now classifies a bareword (`Type::Simple` vs `Term::Name`) from the declarations of the
innermost enclosing block before the unit-wide table, so a lexical enum variant shadows an outer
class of the same name. A `constant` holding a type object (a type name or a
`Metamodel::...HOW.new_type(...)` result) renders as `Type::Simple`, as in rakudo.
