# `my role` gets declaration-site identity

A lexical role used to be registered under its bare name, like `my subset`
before it. Two same-named `my role` declarations in sibling scopes therefore
shared one registry entry, and the last declaration won everywhere:

```raku
my ($a, $b);
{ my role R { method go { "first" } }; $a = R }
{ my role R { method go { "second" } }; $b = R }
say $a.go;   # was "second"; now "first", as in Rakudo
```

The same collapse broke `but` mixins (`42 but M` in two blocks mixed in the
same `M`) and curried lexical roles (`P[1]` from the first block dispatched to
the second block's `P`).

A `my role` now gets the ADR-0047 treatment that `my class` and `my subset`
already had. It is stored under `Name\0<declaration-site id>`, and its bare
name is bound to that storage name for the rest of the declaring scope. Later
same-named `my role` declarations in the same scope reuse that storage name,
so candidates of one parametric group (`my role G[$x] { }; my role G[$x, $y]
{ }`) and a stub with its completion still form one role.

Some code resolved roles by their source spelling, and it now goes through the
lexical env. That covers a class's `does` list, parameterized references
(`does R1[::T]`, `my R1[Int] $x`, `sub f(R1[Int] $x)`), and a role naming
itself in a signature (`method !foo(A:D:)`). `.^name`, `.gist`, `.raku`, and
the ambiguity and class-hierarchy errors show the source name. A curried lexical
role displays as `R1[Int]`. A role's `role_id` now comes from the same counter
as declaration-site ids, so a lexical role's storage key can never look like a
candidate key of an unrelated package-scoped role.

This finishes #9894.
