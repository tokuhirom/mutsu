# A parameter typed `Associative` / `Positional` can have elements assigned

`sub f(Associative $p) { $p<k> = 1 }` died with `Type check failed for an element
of $p; expected Associative but got Int (1)`: the `$`-sigil constraint was read as
the type of the elements. A container type on a `$` variable describes the
container, and the list of container types that constrain nothing lacked the
roles the containers do (`Associative`, `Positional`, `Iterable`, `QuantHash`,
`Baggy`, `Setty`, `Mixy`) and ignored a `:D` / `:U` smiley. Qwiratry's navigators
(`multi method attach-child(Associative $parent where {...}, ...)`) stored every
child through such a parameter, so `TreeWithPath` died on its first attach (#13370).
