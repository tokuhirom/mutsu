# `my int:D $n = 3` is refused, as in rakudo

A definite native type on a scalar declaration (`int:D`, `num:D`, `str:D`) took the assigned value
(`my int:D $n = 3` stored `3`), and `my int:D $n;` was accepted. Rakudo's RakuAST frontend refuses the
boxed `Int`/`Num`/`Str` it is given ("expected int:D but got Int (3)") and asks for an initializer
when there is none. The declaration check now does the same; a native `int:D` *parameter* still
binds the value. Closes #16707.
