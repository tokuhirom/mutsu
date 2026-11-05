# `$!attr := $raw-param` joins the caller's variable on the first call

A `Mu $topic is raw` method parameter bound into an attribute (`$!value :=
$topic`) kept a snapshot of the caller's variable when the call was the first
to reach a top-level lexical, so writes through the attribute never reached it.
The bind now joins the shared cell the binder installed for the argument
(#15708).

`$obj[*]:exists` on an object that is not `Positional` is now a one-element
list, as in Rakudo, instead of asking `EXISTS-POS` for every index of a
user-declared `elems`.
