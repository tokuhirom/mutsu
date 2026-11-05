# `.VAR` of a native variable and of a List item

`my byte $b; $b.VAR.^name` is now `UIntLexRef` (`IntLexRef`, `NumLexRef`, `StrLexRef` for the other
native scalar types), as in Rakudo, and the same holds for a reflected List item that carries the
native cell. `($t,)[0].VAR.of` on a typed variable's cell now answers the cell's own type (`Int`;
`Mu` for an unconstrained one) instead of dying on the value it holds (#15853).
