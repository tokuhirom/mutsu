# `--> CONSTANT` returns the constant's value

A signature return spec that names a declared `constant` (`constant K = 42;
sub f(--> K) {...}`) or the core term `IterationEnd` is a definite return value,
as in rakudo, instead of a return type constraint (which died with "Type check
failed for return value; expected K") or an undeclared type error.

Found via ForwardIterables, whose `method push-all(\target --> IterationEnd)`
now returns `IterationEnd` (all 7 assertions of its suite pass).
