# A typed variable that can never bind fails at CHECK time

`my Str $s = "a"; sub f(Int $x) {}; f($s)` now fails at CHECK time, as it
does in rakudo. The CHECK-time refutation pass (#18874) reads a variable
declared with a core type as that type: every value the variable can hold is
of that type. As for a type object, a supertype (`my Cool $c`) leaves the call
to the run-time binder.

For now the pass reads only a top-level `my`/`our` scalar whose name is
declared nowhere else in the unit. A parameter, or a name that an inner scope
declares again, is still left to run time.
