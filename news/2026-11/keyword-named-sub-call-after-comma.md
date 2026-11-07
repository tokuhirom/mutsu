# A keyword-named sub call after a comma is a call, not a modifier

`say 1, with(1)` (and `put`, and subs named `given`/`when`/`for`/`if`) used to
fail with "expected statement", because the say/put argument list took the
keyword after the comma for a statement modifier. A keyword glued to `(` is now
a sub call there, as in Rakudo; `say 1, if 0;` still parses as a trailing-comma
modifier.
