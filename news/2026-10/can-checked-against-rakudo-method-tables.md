# `.^can` and `.^methods` checked against Rakudo's method tables

`Rat.^can('numerator')`, `Num.^can('isNaN')`, `Int.^can('FatRat')` and
`Instant.^can('Bridge')` all answered `False`, although each call works.
For built-in types, `.^can` reads the `DECLARED` bit and `.^methods` the
`INTROSPECTABLE` bit of the native method row catalog. Those rows were baked
once from Rakudo, and methods added to the native dispatch afterwards never
got one (#11271).

`scripts/gen-rakudo-method-tables.raku` now snapshots Rakudo's
`^method_table` keys and `.^methods` names for every catalog owner into
`src/builtins/rakudo_method_tables.txt`. A unit test checks the catalog
against it on every run. A `DECLARED` bit Rakudo disagrees with fails. So
does a name Rakudo declares or lists, which mutsu's native dispatch
recognizes on a sample instance, but whose row lacks the bit; the failure
prints the row to paste. The 412 rows it reported are added. Most are on
`Rat`, `Num`, `Int`, `Complex`, `Bool`, `Map`, `Capture`, `Instant`,
`Duration`, `Uni` and the Set/Bag/Mix family.

The new `Duration.narrow` row exposed an `i64` overflow: `(now - now).narrow`
panicked in a debug build. Its hand-rolled decimal-digit Num -> Rat conversion
now goes through `real_to_rat`, the conversion `Duration.new` already uses.
Instant arithmetic still builds Num-valued Durations; that is #11273. That
`.^methods` lists inherited names such as `List.map`, which Rakudo does not,
is #11272.
