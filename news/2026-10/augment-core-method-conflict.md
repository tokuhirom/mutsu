# `augment` of a core type rejects redeclaring a method the type declares

`use MONKEY-TYPING; augment class Str { method uc { "x" } }` compiled, and the
augmentation silently replaced the built-in `uc`. Rakudo rejects it with
`Package 'Str' already has a method 'uc' (did you mean to declare a multi
method?)`. Likewise, Rakudo rejects `augment class Str { multi method Int(Str:D:)
{ 1 } }` with `Cannot have a multi candidate for 'Int' when an only method is
also in the package 'Str'` (#10234).

The rule, taken from Rakudo: a plain, `only` or `proto` method cannot join a
method of the same name in the type's *own* method table, whether that method
is a multi dispatcher or not. A `multi` candidate can join a multi, but not an
`only` method. A name the type only inherits stays free: `Array.sort` belongs
to `List` and `Str.FatRat` to `Cool`.

The data lives on the canonical native method rows (ADR-0019). `DECLARED`
already records that Rakudo's `::(owner).^method_table` has the name. The new
`ONLY_METHOD` bit records that the method there is not a multi dispatcher.
`scripts/gen-rakudo-method-tables.raku` now also prints `only` lines into the
committed `rakudo_method_tables.txt` snapshot. The bit was baked from those
lines, and an oracle test checks that no row claims it falsely.

One case is not covered yet: a `proto method` in an `augment` body is
registered by a separate path, so it is not checked.
