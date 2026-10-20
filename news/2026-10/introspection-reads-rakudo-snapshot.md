# Built-in introspection reads Rakudo's method-table snapshot

`.^can`, `.^find_method`, `.^methods`, `.^method_table` and the compile-time
`augment` conflict now answer from `src/builtins/rakudo_method_tables.txt`, the
committed output of `scripts/gen-rakudo-method-tables.raku`, gated by the method
rows (ADR-14032). A name is reported only where Rakudo declares or lists it and a
row implements it; the `augment` conflict reads Rakudo's facts alone, so
redeclaring a method Rakudo declares fails even where mutsu has no row for it.

The hand-kept recognition table (`native_method_row_table.rs`), its oracle
module and the registry's seeded builtin method entries are gone: they were
baked from Rakudo once and had drifted from both Rakudo and the rows. The
snapshot generator now covers every owner the snapshot lists plus any named on
its command line, and records `Failure`'s facts correctly. `Cool.Int` gained the
row Rakudo declares (reached by its owner only, so `UInt.Int` still answers the
type object).
