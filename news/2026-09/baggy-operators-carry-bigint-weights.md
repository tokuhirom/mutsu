# The baggy operators no longer saturate a Bag weight to `i64::MAX`

`BagData.counts` has been a `HashMap<String, BigInt>` for a while, and its doc
comment says why: *"Weights are arbitrary-precision so a BagHash weight can
exceed i64::MAX."* The **operators** did not work in that type. Every one of
them flattened the map down to `HashMap<String, i64>` at coercion time, so
`10**30` became `i64::MAX` before any arithmetic ran — and then the addition one
line later overflowed:

```raku
say (a => 10**30).Bag (+) (a => 1).Bag;
# raku:  Bag(a(1000000000000000000000000000001))
# mutsu: thread 'mutsu-main' panicked at src/vm/vm_set_arith_ops.rs:60:21:
#        attempt to add with overflow
```

A debug build panicked. A release build does not check the overflow, so it
wrapped and returned a silently wrong count — which is worse. And the saturation
was itself a wrong answer even where nothing overflowed:
`(a => 10**30).Bag (==) (a => 10**30 + 1).Bag` was `True`, because both sides
saturated to the same `i64::MAX`.

## What changed

The weight maps are `BigInt` end to end. That is five coercion helpers and the
operator bodies that consume them:

- `Interpreter::coerce_to_bag` / `bag_insert_item` and the `(+)` / `(&)` bodies
  in `src/vm/vm_set_arith_ops.rs` (the panic site);
- `runtime::utils::set_ops`'s `coerce_to_bag`, and the `(-)` / `(&)` / `(^)`
  bodies plus the multi-operand symmetric difference;
- `runtime::utils::set_coerce`'s `resolve_bag_tab_keys` and `to_bag_map` —
  including the `"key\tweight"` tab format, whose embedded weight was parsed
  with `parse::<i64>()` and is now parsed as a `BigInt`;
- `Interpreter::union_bag_counts`, `multiply_bag_counts` and
  `addition_bag_counts` in `src/runtime/ops_set.rs`, feeding `apply_set_union` /
  `apply_set_multiply` / `apply_set_addition` (the reduction path's twins) and
  `apply_set_equality`;
- `Interpreter::value_to_bag_counts` in `src/vm/vm_set_ops.rs`;
- the `:into(Bag)` accumulator in `builtins_collection_classify.rs`.

`bag_counts_as_i64`, the saturating helper all of these went through, has no
callers left and is gone.

One new helper, `runtime::utils::bag_weight`, reads a weight out of a `Value` at
full precision. It replaces the `v.to_f64() as i64` spelling that appeared at
about a dozen sites, and keeps raku's coercion rules: a bag weight is an `Int`,
so `(a => 2.7).Bag` is `("a"=>2).Bag` (truncating toward zero, as `Rat.Int`
does), a `Bool` weighs 1/0, and a negative weight drops the element.

The one place a weight is still clamped is `.kxxv`, which expands a bag into one
element per unit of weight — materializing `10**30` elements is not something a
wider integer would help with, and raku hangs on it too.

`t/bag-bigint-weights.t` grows from 12 assertions to 36: all six operators at
`10**30`, the mixed-operand coercion routes (against a list, a `Hash`, a bare
`Pair`), the `BagHash` spelling, the `[(+)]` / `[(-)]` / `[(.)]` reduction
entry points, a multi-key symmetric difference, `(==)` on weights one apart, and
the small-weight and coercion-corner cases that must not have moved. Every
expectation was measured against raku v2026.07 first.
