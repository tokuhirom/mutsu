# A local alias of an `is raw` parameter keeps the caller variable's type

`sub f(Mu $c is raw) { my $l := $c; $l = 7 }` called with a `my Str $s` wrote `7` into `$s`
unchecked: the shared cell the caller's variable was boxed into carried no declared type, and the
alias bind reused it as is. The bind now records the source variable's declared type on a cell
that has none, so writes through the alias (and through an attribute bound to the variable) die
with `X::TypeCheck::Assignment` as in Rakudo. Closes #14694.
