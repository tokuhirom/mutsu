# EVAL($ast): `.=` declarations, definiteness smileys and prelude terms

Three `EVAL($ast)` gaps, each checked against rakudo (#16356):

- `my Int:D \b .= new: 42` constructed `Int:D` (the smiley qualifies the variable,
  not the constructed type) and failed with "You cannot create an instance of this
  type"; the lowering now strips it, as the parser does.
- A parenthesised anonymous `.=` declaration (`(my Int $ .= new: 42)`) lowered to
  a one-element list instead of the value; it is now the same single expression
  declaration a named one lowers to.
- A builtin provided from a prelude (`Rational[Int,Int].new(3, 10)`) was not
  switched on by a lowered tree, because the names the gates read from a tree
  skipped bare type names in term position and the modules the unit loads; both are
  read now.

Eighteen more `t/` files join the `EVAL(slurp($file).AST)` ratchet.
