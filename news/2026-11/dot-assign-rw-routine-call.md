# `f() .= meth` assigns through an `is rw` routine's container

`.=` on a routine call evaluated the call into a fresh variable, so the result
of the method never reached the container an `is rw` routine returns:
`sub f() is rw { $s }; f() .= uc` left `$s` unchanged (#19024).

The temporary is now bound to what the call returns, and the method's result is
assigned through it. `f() = f().uc` behaves the same way, and the call is still
evaluated once. A routine that returns a plain value dies on the assignment, as
on rakudo. rakudo's message names the value, while mutsu's is the generic
"Cannot assign to an immutable value".
