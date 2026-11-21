# RakuAST: method-body terms, glued prefix operands, object hashes

More `.origin` and shape gaps closed (#15780):

- the terms of a package body's methods keep their origins when the same term
  is written twice in one statement (`$!x ~ $!x`, `&!g() ~ (&!g).WHAT`).
  `nested_methods::unhoist` copied every statement of a class body before
  conversion, so no term sat at the address its origin was recorded at and
  only a term unique in its statement could be matched by shape; a body with
  nothing hoisted is now borrowed as written;
- a variable glued to a prefix operator (`⚛$n`) is placed
  (`operator_origins::glued_variable_operand`);
- an object hash (`:{ 1 => 2 }`, `:{}.elems`) starts at its `{`, as in
  Rakudo, and the `ArgList` or `Initializer` holding it is placed;
- an attributive parameter target (`submethod BUILD(:&!f)`) visits the
  `Var::Attribute` it binds, not a `VarDeclaration::Simple`.
