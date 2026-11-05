# A `does` statement is not sunk, a `but` statement is

rakudo sinks a statement `X but R;` (the composed `sink` runs) and does not sink `X does R;`,
whatever X is: `does` mixes into its target. mutsu sunk both, so `[<a b c>] does role { method sink
{ ... } };` and `(5) does R;` ran a `sink` rakudo never calls, and the documentation idiom
`($b + 1) does role { method sink { $b++ } };` incremented `$b` where rakudo leaves it at 0. Only
`but` is sunk as a statement now; a routine's result, `does` mixin or not, is still sunk by its
caller (#16511).
