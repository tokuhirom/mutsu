# `&name` inside a method prefers the class's own `my method`

A `&STORE` (or any `&name`) looked up inside a routine resolved to a same-named
sub registered globally before it consulted the lexical `my method` the
enclosing class body carries. A program that declared `only STORE` in one sub
and `my method STORE` in a `Proxy` subclass therefore handed the wrong callable
to `callwith :&STORE`. The scope's own lexical routine now wins.
