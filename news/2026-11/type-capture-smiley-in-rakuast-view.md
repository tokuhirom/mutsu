# A type capture's smiley survives in the RakuAST view

#19042 made `sub f(::T:D $x)` check its argument's definedness, as rakudo
reads it as `::T Any:D $x`. It did so by turning the smiley into an `Any:D`
parameter type. So `.AST` showed `Any:D ::T $x`, where rakudo shows
`::T:D $x` with the smiley on the `Type::Capture` node and the parameter's
type left at the default `Any` (#19020). `:_` was dropped entirely.

The parameter now also records the capture's own smiley. The view puts it on
the `Type::Capture` node and leaves the type at the default. Lowering an
`.AST` back into code restores the definedness check, so `EVAL` of the tree
still refuses `f(Int)`. An explicit `::T Any:D $x` keeps its written type.
