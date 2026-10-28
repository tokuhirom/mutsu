# `IO::Path.add/child/sibling` stringify an object argument through its `Str`

`"x".IO.add($obj)` printed the object's default rendering (`x/Foo<28>`) instead of
calling its user `Str`, and did not die when that `Str` answered the object itself
("This type cannot unbox to a native string"). The dispatcher now resolves an
instance or mixin segment to the string its `Str` answers before the pure rows
run (#15185).
