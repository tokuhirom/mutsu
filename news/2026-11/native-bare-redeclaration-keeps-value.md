# A bare `my int $x;` redeclaration keeps the value

Redeclaring a same-scope variable without an initializer is a no-op in Raku. For
native types (`int`, `num`, `str`, `uint8`, ...) mutsu reset the variable to the
type's zero, because the parser's synthesized native default was not recognised
as a default initializer. It is now, so `my int $x = 7; my int $x;` keeps 7.
