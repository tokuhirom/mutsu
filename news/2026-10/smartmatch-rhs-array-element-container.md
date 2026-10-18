# Smartmatch against an array element returned by a method calls its ACCEPTS

`5 ~~ @a.head` (and `.tail`, `.pick`, ...) right-hand sides arrive as the element's Scalar
container; the smartmatch op did not read through it, so a user class's `multi method ACCEPTS`
was never reached and the match answered False. VERS's `$topic ~~ @ranges.head` was the
real-world case; its `t/01-basic.rakutest` now matches Rakudo.
