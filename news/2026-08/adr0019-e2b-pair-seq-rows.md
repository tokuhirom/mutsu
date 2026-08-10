# ADR-0019 E2b: Pair and Seq join the native-row catalog

A second slice of ADR-0019's `native_call_unmodeled`-to-zero campaign
(box E2b). Unlike the first eleven owners the row catalog covers, `Pair`
and `Seq` have no `builtin_type_method_names` entry to draw a candidate
list from -- `builtin_type_method_names`'s match falls to its `_ =>
Vec::new()` default for both. So the row set was built the same way the
original catalog was: probe a curated candidate list (drawn from the
`Seq`/`Pair` type docs plus the `native_call_unmodeled` sweep breakdown)
against a real `Value::pair`/`Value::seq` sample and record what the
cascade actually recognizes.

67 rows landed across the two owners -- accessors (`key`, `value`,
`antipair`), stringification (`Str`, `gist`, `raku`, `WHICH`), and the
list-coercion surface both types share with `List` (`join`, `elems`,
`head`, `tail`, `pick`, `roll`, `batch`, `flat`, ...). `so`/`not`/`defined`
were deliberately left out since the prior E2b slice's chain-walk fix
already covers them via the `Any` row. A new
`pair_seq_rows_are_backed_by_the_cascade` test is the inverse-probe half
of the same discipline the original 11-owner catalog uses: every non-
`SPECIAL` row must be confirmed against the real cascade, not just
asserted.

A fresh `MUTSU_VM_STATS=1` sweep over the full `t/` suite confirmed the
result: `native_call_unmodeled` dropped from 12154 to 8654, a further 29%
reduction (77% cumulative from the original 37904 baseline before this
E2b campaign started). Every `Pair`/`Seq` entry disappeared from the
breakdown's top hitters.

The largest remaining cluster is `Match` (~1700 hits across `ast`, `Str`,
`made`, `chars`, `values`), deferred since building a representative
`Match` sample is more involved than a plain value constructor -- a real
regex match carries capture state that `Value::pair`/`Value::seq` didn't
need. That, along with the smaller `Array`/`List`/`FatRat`/exception-type/
`RakuAST::*` remainders, are the next E2b sub-slices.
