# A type-object argument that can never bind fails at CHECK time

`sub f(Int $x) {}; f(Str)` now fails at CHECK time, as it does in rakudo:
"Calling f(Str) will never work with declared signature (Int $x)". The
CHECK-time refutation pass (#18874) treats a core type object as a static
argument. As in rakudo, a type object that is a supertype of the parameter's
type still reaches the run-time binder, since it may name a value that binds:
`f(Cool)`, `f(Any)` and `f(Mu)` for an `Int` parameter. A type the unit
declares itself is left to the binder too.
