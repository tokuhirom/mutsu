# A placeholder in a `start`/`do`/`try`/`once` body inside a routine is a compile error

`start { $^c }` was rejected only in the mainline, and only when the statement
ran; inside a routine body it compiled silently (and `sub f { start { $^c } }`
even gave `f` a placeholder parameter). The parser now raises
`X::Placeholder::Block` for a placeholder used directly in a `start` block (`%_`
stays legal, as in a method), and the unit compile (and `EVAL`) rejects the same
in a `do`/`try`/`once`/`gather` body anywhere, routine bodies included, as rakudo
does. The `StartBody` parsed-expression variant that carried the old runtime
rejection is gone.
