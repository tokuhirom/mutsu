# `$x\n    .method(...):g` keeps its adverb

A method call written on its own line after the invocant is remembered as a dotty infix call; that
wrapper hid the call from the postfix-adverb check, so `:g` was dropped (as a `say` argument) or
died "Two terms in a row" (in an initializer). Found loading JavaScript::D3, whose 7 test files now
match `raku`.
