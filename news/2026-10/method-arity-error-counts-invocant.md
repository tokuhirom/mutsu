# A method's arity error counts the invocant

`P.new.m(1, 2)` on `method m($x)` now says "Too many positionals passed; expected 2
arguments but got 3" (and the singular "expected 1 argument" for a parameterless
method), as Rakudo does: the implicit `self` is part of both numbers. The binder
already knew it was binding a method (`bind_self_lexical`); the two arity messages now
use it (#14565).
