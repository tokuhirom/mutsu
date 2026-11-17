# `my \r = f($x)` aliases the container an `is raw` routine returns

A sigilless term initialized by a routine call now binds whatever the call
hands back. If the routine is `is raw` or `is rw` and returns its `is rw`
parameter, that is the caller's container, so the term aliases it, as on
rakudo: `sub g($x is rw) is raw { $x }; my \r = g($y); r = 5` writes `$y`.

Any other return is a value, and the term stays readonly. A call initializer
now takes the same run-time verdict of its bind source that a method call or
a subscript initializer already did. Before, every call was compiled as a
readonly term, so the assignment died with "Cannot modify an immutable Int".
