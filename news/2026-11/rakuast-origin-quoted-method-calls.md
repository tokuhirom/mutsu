# `.origin` of quoted method calls

A quoted method call (`$x."uc"()`, `$x.'lc'()`, `$x."substr"(1, 2)`,
`$x.?"uc"()`, `self!"p"(1)`, `$x."$name"()`) now carries the ranges rakudo
2026.11 gives it:
- the `Call::QuotedMethod` runs from the opening quote of its name to the
  call's end;
- the name's `QuotedString` is the quoted text, and its segments are placed;
- the `ArgList` is the inside of the parentheses.

`Call::QuotedMethod` also now declares its `name` and `args` fields like the
other calls. So an empty argument list is a visited child carrying its range,
an empty node at the `)`, as on rakudo (#15780).
