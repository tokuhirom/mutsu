# Range operators now work as `Z`/`X` metaop bases

`(1, 2) Z.. (5, 6)`, `X..`, `Z..^`, `Z^..`, and `Z^..^` are legal Raku — the
range operators are valid metaop bases, and the parser already accepted them
(`..`, `..^`, `^..`, `^..^`, `...`, `...^` have been in `parse_meta_op`'s base
list from the start). But at runtime, `Z`/`X` handed the base operator to the
*reduction* operator table, which only knows how to fold two operands and has
no entry for building a Range — so every use died `Unsupported reduction
operator: ..`.

Fixed by extracting the Range-construction logic out of the dedicated
`MakeRange`/`MakeRangeExcl`/`MakeRangeExclStart`/`MakeRangeExclBoth` opcode
handlers (`src/vm/vm_misc_typed_range.rs`) into pure `Value -> Value`
builders, and calling them directly from `eval_reduction_operator_values`
(`src/vm/vm_dispatch_helpers.rs`) for the `..`/`..^`/`^..`/`^..^` operator
strings — before they ever reach the reduction table.

```
(1, 2) Z.. (5, 6)      # (1..5 2..6)
(1, 2) X.. (5, 6)      # (1..5 1..6 2..5 2..6)
(1, 2) Z^..^ (5, 6)    # (1^..^5 2^..^6)
```

The sequence operator (`...`/`...^`) as a `Z`/`X` base was investigated too,
but raku does not actually apply it as a base operator there — `(1,2,3)
Z... (10,20,30)` falls back to plain pairing (`((1 10) (2 20) (3 30))`)
rather than expanding a sequence per pair, so it was deliberately left out
of scope for this fix.

Regression test: `t/zip-cross-range-metaop.t`.
