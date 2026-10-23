# `$x := $_` in a `for @a` loop keeps the element alias

A scalar bound to the implicit topic of `for @a` used to receive the element's value, so a later write to the element was not visible through it (`@e[1] = 20; say $bound` printed the old value). `WrapVarRef` now keeps the topic's raw container, as it does for a captured outer lexical, so the bind aliases the element as in Rakudo (#14758).
