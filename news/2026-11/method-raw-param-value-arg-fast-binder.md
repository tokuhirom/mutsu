# A method's `is raw` parameter binds a literal argument on the fast binder

A method call such as `$o.m(1)` or `$o.m($n + 1)`, where no argument is a variable, used to bind a
`$x is raw` parameter through the general signature binder. A plain `$x` or a sigilless `\x`
parameter took the fast binder for the same call. The compiler emits no argument-source table for
a call site whose arguments name no variable, and the fast path's container-parameter check read a
missing table as "unknown". It then left every `is raw` parameter to the general binder.

With no table, the general binder has no source name either. It aliases a `ContainerRef`
argument's cell, finds a variable only through a `VarRef` wrapper, and otherwise binds the value
read-only. The fast path's value classifier makes the same three binds (and declines a `VarRef`).
So a missing table now reads like a table that marks the argument as no variable.

`class A { method c(Mu $x is raw) { $x } }; $a.c(1)` went from about 6.9 µs to 3.3 µs per call
on a release build, against 2.1 µs for `Mu $x` without the trait. The remaining gap is the
method-call plan, which still declines every container-aliasing parameter.
