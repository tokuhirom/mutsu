# RakuAST model methods are visible to `.^can` and friends

`$sub.^can("WHY")`, `.can("replace-body")`, `.^find_method` and `.^lookup` answered nothing for the
methods the RakuAST model serves from native rows, although calling them worked. Reflection walked a
class's own rows and its parents' but not the roles it does, and a few rows were filed on one class
(`Block`) while rakudo declares the method on its siblings too. `inherited_method_names` now also
takes the rows of every role of the class (`Doc::DeclaratorTarget`'s `WHY`/`set-WHY`),
`has-placeholder-parameters` is filed on its real owner, the `PlaceholderParameterOwner` role (so
`Class` no longer reports it), and `replace-body` has rows for `Sub`, `Method` and `Submethod`
beside `Block`. Closes #16805.
