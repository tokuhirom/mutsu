# A quoted pseudo-method call on a variable answers

`$x."WHICH"()`, `$x."WHERE"()`, `$x."DEFINITE"()` and `$x."WHAT"()` threw `X::Method::NotFound` when the receiver was a
variable, though the same call worked on a literal and as a bareword. A quoted call of one of these names skips the
built-in answer so that a class's own method of that name can run, but on a variable it skipped it for every receiver.
It now skips it only when the class declares such a method, as the literal-receiver path already did (#13055).
