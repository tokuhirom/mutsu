# A mutating method on `($x = EXPR)` writes the assigned scalar

In Raku an assignment expression evaluates to the assigned scalar's
container, so a receiver-mutating method called on it writes that scalar:
`($x = "cd").subst-mutate(/c/, "Z")` leaves `$x` as `Zd`, and
`($ = "1234567").subst-mutate(:g, /../, "XX")` works on the anonymous state
scalar. mutsu refused both ("require mutable arguments"), because the call
had no variable name to write through.

The compiler now recognizes an assignment to a scalar as a method receiver,
via `Compiler::assigned_scalar_name`, and sends the call through the named
variable path, as it already did for `(my $x = ...).method`. The
`subst-mutate` subtests of roast `6.c/APPENDICES/A03-older-specs/01-misc.t`
now all pass; the file's remaining failure is `Pair.freeze` identity.
`($x = 6)++` has the same root cause in the increment operators and is filed
as #18840.
