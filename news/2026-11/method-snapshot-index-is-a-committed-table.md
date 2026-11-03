# The Rakudo method snapshot is no longer scanned on the first method call

`method_introspection::facts()` indexed the 1.26 MB `rakudo_method_tables.txt` by owner on the
first introspection call of every program, and `str::lines` over it cost ~2.05M instructions
(7% of `bench-grammar-parse`). Where each owner's lines are is now a committed table
(`method_snapshot_index_gen.rs`, written by `scripts/gen-method-snapshot-index.py`, checked by the
`method_snapshot_index` unit test), looked up by binary search; an owner's lists are still parsed
on first use. `bench-grammar-parse` (callgrind, profiling build): 28.91M -> 26.84M Ir (-7.2%).
