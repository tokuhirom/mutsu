# `.origin` of `$f.(...)` and the empty `ArgList` of `$f()`

`Call::Term` of a dotted call (`$f.(1, 2)`, `&f.()`) carried no origin, and the empty `ArgList`
of `$f()` / `$f.()` was neither visited by `visit-children` nor placed. Rakudo starts the
`Call::Term` at the `(` after the dot and puts the empty `ArgList` just inside the
parentheses; both now match (#15780).
