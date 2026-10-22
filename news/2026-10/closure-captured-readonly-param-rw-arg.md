# A closure refuses a captured readonly parameter for an `is rw` parameter

`sub f($p) { my &c = { g($p) }; c() }` with `sub g($v is rw)` now throws
`X::Parameter::RW`, like the routine's own body does. A nested closure's compiler
inherits the enclosing routines' readonly `$` parameters, minus any name it
rebinds itself (#14209).
