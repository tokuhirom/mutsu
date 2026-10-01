# Class body statements run in source position again, ahead of a BEGIN

`say 1; class B { say 2 }; BEGIN say 3` printed `2 3 1`: the BEGIN prologue (ADR-0134) moved the whole
class declaration ahead of the mainline, so the `say 2` in its body ran with the BEGIN-time effects. Rakudo
composes the class at BEGIN time but runs a body's bare statements at run time, in source position, and
prints `3 1 2`.

A class, grammar, `module` or `package` declaration the prologue takes is now split. The prologue keeps the
declaration with its attributes, methods, subs, nested types and the static half of each variable. The bare
statements and the variables' initializers stay where they were written, in a `Stmt::PackageRuntimeBody` that
re-enters the package through `PackageScope`. The body's lexicals are bound from the package's static store
there, so a BEGIN sees `class A { my $c = 5 }`'s `$c` as `Any` and the mainline sees `5`, as on Rakudo.

The same work fixed a bug that did not need a BEGIN at all. In `class A { my $c; $c = 0; method inc { $c++ } }`,
the later assignment compiled to a package variable `$A::c`. That variable detached `$c` from the class's
methods, and `A.inc` returned `0` on every call. A body's own `my` lexicals are no longer package-qualified.
