# `infix:<=:=>($a, $b)` called as a function is container identity

`&infix:<=:=>($a, $b)` and `&[=:=]($a, $b)` passed their arguments by value, so
two distinct variables holding equal values answered `True`. A direct call is
now lowered to the `=:=` operator form, which sees the variables' containers
(as the `=`/`:=` call forms already were). Calling the operator through a code
object (`my &f = &infix:<=:=>; f($a, $b)`) still compares values; that remains
open on #16948.
