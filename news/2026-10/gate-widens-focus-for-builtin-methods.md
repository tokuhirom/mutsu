# The quick gate widens its focus for a built-in method change

`scripts/dev gate` in the quick profile now adds `t/oo`, `t/types` and `t/collections` to the
files it runs when the branch changes `src/builtins/method_table/`, `methods_0arg/`, `methods_narg/` or
`native_method_row_table.rs`. A built-in method's callers are everywhere, and the method-row work
regressed two untouched tests that only CI caught. ADR-0126 has the amendment.
