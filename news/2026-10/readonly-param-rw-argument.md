# A readonly parameter is not an `is rw` argument

`sub f($p) { g($p) }` with `sub g($v is rw) { $v = 'x' }` used to bind `g`'s
`$v` to `f`'s readonly `$p` and write through it, so `f(1)` answered `x`.
Rakudo refuses the bind with `X::Parameter::RW`: a plain `$` parameter is a
value, not a writable container.

A call argument that names a readonly `$` parameter of the enclosing routine
is now passed as a value, with no container tag for an `is rw` parameter to
bind, so the binder refuses it the way it refuses a literal. This applies to
positional and named arguments and to multi forwarding. The parameter is
identified at compile time from the routine's own signature. A body `my`, or
an inline loop or block parameter that reuses the name's slot, makes the name
writable again, so a shadowing writable binding is never refused (#13782).
