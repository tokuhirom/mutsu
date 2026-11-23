# A method with a definite return value refuses `return $x`

A routine whose signature names a definite return value (`--> Nil`,
`--> True`, `--> 42`) may not also `return` an argument in its own scope.
Rakudo refuses that at compile time with an `X::Comp::AdHoc` ("No return
arguments allowed when return value ... is already specified in the
signature"). mutsu already did this for a `sub`. A method compiled, and the
definite value silently replaced the returned one.

Method registration now makes the same check, using the same own-scope scan:
a bare `return` is fine, and a `return` in a nested block is not checked, as in
rakudo (#18990). This covers class, role and private methods.
