# `::('$x')` finds a lexical scalar again

A symbolic lookup of a variable by name, `::('$x')` or `::($name)`, now
finds a lexical scalar even when it is the program's only reflective
access. It compiles to the indirect type lookup op, which the compiler did
not count as reading a lexical by name. With nothing else (an `EVAL`, a
pseudo-package) to tell it otherwise, the compiler let scalar stores skip
the env the lookup reads. `my $s = 1; say ::('$s')` died with "No such
symbol", and so did a closure reading a lexical of its exited block this way
(#13595). A constant type name such as `::('Int')` still counts as a plain
type lookup.
