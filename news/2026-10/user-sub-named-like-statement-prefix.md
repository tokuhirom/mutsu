# A user `sub race(...)` is called by `race(5)`

`race`, `lazy`, `hyper` and `eager` are statement prefixes in the parser, ahead of
any routine lookup, so `race(5)` became the method form `(5).race` even when the
program declared `sub race(Int $i)` ("No such method 'race' for invocant of type
'Int'"). None of the four is a core routine in rakudo, so a call written with
parentheses now goes to a declared sub of that name (the same guard `so(` already
had); `race for ...`, `race LIST`, `eager LIST` and the others are unchanged.
