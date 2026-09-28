# Prettier::Table passes: Positional-typed `$` params and destructure lexicals

Prettier::Table 1.1.3 now passes all 13 of its test files (it was 6/13). Two
interpreter bugs were behind the 7 failing files. Neither is specific to the
distribution.

**A `Positional`-typed `$` parameter no longer gets a Scalar wrapper.** Rakudo
wraps a non-`is copy` `$` parameter in a read-only Scalar only when its
nominal type could be Iterable (`lower_signature` in `Perl6/Actions.nqp`).
`Positional $x`, `Associative $x` and `PositionalBindFailover $x` bind the
argument decontainerized. So `sub f(Positional :$a) { my @x = $a // ... }`
flattens an Array argument. mutsu itemized every `$` parameter, which turned
Prettier::Table's column alignments into one element and cut the header rule
off after the first column.

The binder's per-parameter precompute is now a three-way `ScalarParamBind`
(itemize, keep, decontainerize). The compiler reads the same mode, so
`my @a = $x` and `for $x` do not re-itemize such a parameter because of its `$`
sigil.

**Destructured sub-signature targets are declared lexicals.** A positional
`-> ($a, $b)` target used to be lowered to a by-name assignment, and inside a
routine that wrote an undeclared env name. A caller's same-named readonly
binding therefore leaked in: `for <x> -> $field { $t.get-string }` made the
method's `-> ($field, $width, $align)` die with "Cannot assign to a readonly
variable or a value". The targets are now declared like the multi-parameter
pointy-block ones. `@`/`%`, `is raw` and `is rw` targets bind the element's
container, so `%a is raw` keeps its `is default`.
