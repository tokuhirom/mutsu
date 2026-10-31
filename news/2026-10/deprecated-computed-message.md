# `is DEPRECATED(EXPR)` evaluates its message when the routine is declared

`is DEPRECATED($msg)`, `is DEPRECATED("a" ~ "b")` and `is DEPRECATED(f())` on a
`sub` or a `method` keep their message expression instead of the argument's
source text, and the message is the expression's value when the declaration
runs, as rakudo applies the trait. A unit that used one (Algorithm::LibSVM)
used to be refused by the RakuAST round trip. A sub's hoisting pass leaves the
message to the in-sequence declaration, which is the first point the
expression's lexicals exist.

Algorithm::LibSVM 0.0.20 now loads; its `t/07-parse.t` (the only file rakudo
passes without the libsvm shared library) matches rakudo, 14 ok.
