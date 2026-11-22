# `&name` of an undeclared routine is a compile-time error

In rakudo, `say &nosuch` fails to compile with `Undeclared routine: nosuch
used at line 1`, the same diagnosis as the call `nosuch()`. mutsu's
undeclared-routine check only recorded calls and barewords, so a `&name`
reference to a routine nobody declared ran and answered `Nil`.

The check now records a plain-identifier `&name` like a call, under the same
rules: declared later in the unit, imported, built-in, a lexical `&` variable
or parameter all resolve. It is more conservative than a call: a `&name` whose
routine the unit declares anywhere (inside a package or a routine body too) is
not judged, since such a routine is reachable through more than the call's
scope rules model. `&infix:<+>`, `&?ROUTINE`, `&*dynamic` and
`&Pkg::name` are left to their own lookups.
