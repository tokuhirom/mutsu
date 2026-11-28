# A supertype-typed variable argument reports the binder's error

`sub f(Int $x) {}; my Cool $s = "a"; f($s)` died with "Calling f(Str) will never
work" at run time, where rakudo reports the binder's "Type check failed in
binding to parameter". A call site now carries each typed-variable argument's
declared type (`StaticArgTypes`), and the run-time refutation classifier applies
the same supertype rule it already used for type-object arguments, so only a
declared type unrelated to the parameter is reported as the compile-time
refutation.
