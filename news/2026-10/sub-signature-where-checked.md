# A `where` clause inside a sub-signature is now checked

The binder now runs the `where` constraint of a parameter nested in a
sub-signature (`*@ ($x where { ... })`, `@ ($x where * > 0)`), so a failing
predicate raises `X::TypeCheck::Binding::Parameter` and a side-effecting
predicate runs, as in Rakudo (#10989).
