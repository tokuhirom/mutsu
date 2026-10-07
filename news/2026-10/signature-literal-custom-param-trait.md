# Custom parameter traits apply to Signature literals

A user `multi trait_mod:<is>(Parameter:D $param, :$tagged!)` now runs for the
parameters of a `:(Int $x is tagged)` literal, as it already did for a `sub`'s
parameters. The parser used to freeze the literal into a constant before any
trait could run; a literal carrying a custom parameter trait is now built at
run time from a routine with the same parameters (#12205).
