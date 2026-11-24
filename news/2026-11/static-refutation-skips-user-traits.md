# A routine with a user trait is not refuted at CHECK time

The CHECK-time refutation pass (#18874) refuted the Deps distribution's own
test: `sub do-background-stuff(UserIdStorage $storage is copy) is injected`
called as `do-background-stuff` failed to compile with "Calling
do-background-stuff() will never work". Deps' `is injected` trait runs at
BEGIN time and marks every parameter of the routine optional, so the call
binds; rakudo sees the rewritten signature and accepts it.

The pass cannot see what a trait does without running it, so a routine with
a trait the unit or a module defines (on the routine or on one of its
parameters) is now left to the run-time binder. Core traits (`is copy`,
`is rw`, `is raw`, `is readonly`, `is DEPRECATED`, `is test-assertion`,
`is implementation-detail`) still leave the signature as written and are
judged as before.
