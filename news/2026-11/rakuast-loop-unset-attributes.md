# RakuAST: the unset attributes of a loop node

`$w.setup`, `.condition` and `.increment` of a `RakuAST::Statement::Loop` (and
the `while` / `until` / `repeat` forms) died "No such method" when the loop did
not carry them, although `.^can` said they exist. They now answer the
attribute's type object, `RakuAST::Expression`, as rakudo does. `static`
0.0.1's analyzer reads exactly these (#16467).
