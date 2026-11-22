# `&name` of an undeclared routine is a compile-time error

In rakudo, `say &nosuch` fails to compile with `Undeclared routine: nosuch
used at line 1`, the same diagnosis as the call `nosuch()`. mutsu's
undeclared-routine check only recorded calls and barewords, so a `&name`
reference to a routine nobody declared ran and answered `Nil`.

The check now records a plain-identifier `&name` like a call, under the same
rules: declared later in the unit, imported, built-in, a lexical `&` variable
or parameter all resolve. `&infix:<+>`, `&?ROUTINE`, `&*dynamic` and
`&Pkg::name` are left to their own lookups.
