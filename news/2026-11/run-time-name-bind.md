# Binding through a run-time name

`$::($p)::A::x := v`, `::($p)::A := v` and `&::($p)::f := &g` now bind. The
last segment of the name is bound in the stash of the package that the
leading segments name, and a leading `OUR` means the current package. The
bind goes through the same package-stash bind as `Foo::{$k} := $v`, so the
target shares the source variable's container. Before this change the parser
turned such a bind into a block that evaluated both sides and bound nothing.
The left side's lookup `Failure` was then sunk and thrown, which is how roast
`S02-names/pseudo-6d.t` and `6.c/S02-names/pseudo-6c.t` died after 56 tests
on the `::($our)::A44 := class { ... }` line (#18742).

A leading `OUR` in a run-time name now also means the current package for an
assignment (`$::('OUR')::A::x = 1`), a sigiled read and a type lookup
(`::('OUR')::A`), as the literal `$OUR::A::x` already did. A package that only
a qualified store autovivified (`$Foo::x = 1`) is found by `::('Foo')`.

rakudo refuses all of these binds ("Cannot bind to pseudo-package" at compile
time, "This case of binding is not yet implemented" at run time), while roast
specifies them as working (todo'd for rakudo). mutsu follows roast. A name
that no package stash holds (an unqualified name, or one under `MY`,
`CALLER`, `DYNAMIC` and the other lexical or dynamic pseudo-packages) still
raises rakudo's "not yet implemented".

Both pseudo files now run to the end (13 failures each, down from aborting
at test 57), and `S02-names/pseudo-6e.t` runs 188 of its 202 tests instead
of 77. Pin: `t/vm/scope/pseudo-our-indirect-bind.t`.
