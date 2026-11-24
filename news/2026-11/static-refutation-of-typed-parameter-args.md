# A typed parameter passed on to an incompatible routine fails at CHECK time

`sub f(Int $x) {}; sub g(Str $s) { f($s) }` is now a CHECK-time compile
error, as in rakudo: "Calling f(Str) will never work with declared signature
(Int $x)", even though `g` is never called. The CHECK-time refutation pass
(#18874) now reads a scalar parameter declared with a core type (a `sub`,
`method` or pointy-block parameter, a loop variable) as that type's type
object, and does the same for a typed `my` variable in a nested block, not
only at the unit's top level. A smiley (`Str:D`) still means `Str`. A
coercion parameter (`Str() $s`) and a supertype of the callee's parameter
type (`Cool $s` for an `Int`) are left to the run-time binder, as in rakudo.

The pass only uses a variable whose name is declared once in the whole unit
(parameters, variables and attributes alike); a name declared twice is left
to the binder.
