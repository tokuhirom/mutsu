# `(+)` on a Bag with a weight above `i64::MAX` panics the VM

Found 2026-09-06 while sweeping the neighbourhood of the Mix-weight numeric-tower
fix (`news/2026-09/mix-weight-arithmetic-under-the-numeric-tower.md`). Not caused
by that change — the panicking line is untouched by it.

## Repro

```raku
say (a => 10**30).Bag (+) (a => 1).Bag;
# raku:  Bag(a(1000000000000000000000000000001))
# mutsu: thread 'mutsu-main' panicked at src/vm/vm_set_arith_ops.rs:60:21:
#        attempt to add with overflow
```

A debug build panics. A release build does not check the overflow, so it wraps
and returns a silently wrong count instead — which is worse.

## Root cause

`BagData.counts` is `HashMap<String, NumBigInt>`, and its doc comment says so
explicitly: *"Weights are arbitrary-precision so a BagHash weight can exceed
i64::MAX."* But the baggy operators do not work in that type. Every one of them
first flattens the Bag down to `HashMap<String, i64>`:

- `Interpreter::coerce_to_bag` (`src/vm/vm_set_arith_ops.rs`) via
  `runtime::utils::bag_counts_as_i64`, feeding `exec_set_addition_op`'s
  `*e += v` at `src/vm/vm_set_arith_ops.rs:60` — the panic site;
- `runtime::utils::set_ops::coerce_to_bag` and `resolve_bag_tab_keys`
  (`src/runtime/utils/set_coerce.rs`), feeding `set_diff_values`,
  `set_sym_diff_values` and `set_sym_diff_multi`;
- `Interpreter::addition_bag_counts` and `multiply_bag_counts`
  (`src/runtime/ops_set.rs`), feeding the `apply_set_addition` /
  `apply_set_multiply` twins used by the reduction path.

So the saturation happens at coercion (`10**30` becomes `i64::MAX`) and the
overflow one line later. `(-)`, `(&)`, `(^)` and `(.)` all share the defect;
`(.)` will overflow far sooner, since it multiplies.

## Why this is not a one-liner

`saturating_add` would remove the panic and keep a wrong answer, which is not a
fix. Doing it properly means carrying `BigInt` through all five coercion helpers
and the four operator bodies above, including the `"key\tweight"` tab-format
resolution in `resolve_bag_tab_keys` (which parses the embedded weight with
`parse::<i64>()`) and the `Value::bag_typed` constructors that take an `i64`
map. It is a coherent slice of its own — do it as one, not bolted onto an
unrelated fix.

`t/bag-bigint-weights.t` already pins that a Bag can *hold* such a weight; the
gap is only in the operators. Extend that file when this is fixed.
