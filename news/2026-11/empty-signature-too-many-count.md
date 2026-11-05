# Calling an empty-signature routine reports how many arguments were passed

`sub f {}; f(-2)` died with "Too many positionals passed; expected 0 arguments but got more",
a fixed placeholder. Rakudo names the count (`... but got 1`). The error now counts the
positionals the call passed (a named `Pair` is still reported by name), through the same
`arity_message` wording as the other arity errors. Found by the raku-doc differential
(`Language/testing.rakudoc:234`, `Language/capture.rakudoc`).
