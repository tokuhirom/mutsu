# `&CORE::name` and `CORE::<&name>` run the core routine, not the program's

A program that declares `multi infix:<eqv>(Foo, Foo)` or `sub abs($x)` got its own routine
back from `CORE::<&infix:<eqv>>` / `&CORE::abs`: the pseudo-package lookup returned a by-name
routine reference, and the call resolved the name again, user declarations first. When a
declaration of the name exists, `&CORE::name` / `&SETTING::name` now build a code value tagged
with the built-in routine's name, and calling it runs the built-in (`call_core_routine`):
operators through `call_infix_routine`'s core-only mode (user candidates, the `&infix:<op>`
env callable and the by-name expression fallback skipped, `eqv` through `core_eqv_values`),
`Cool` routines through the built-in arms and their invocant's method. An override that
delegates to the core operator (Red's `Red::Operators`) therefore no longer re-enters itself
(#15168).
