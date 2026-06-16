# Plan: arbitrary-precision Bag/BagHash weights (`BagData.counts` i64 → BigInt)

Next-session playbook for the **last remaining `S02-types/baghash.t` failure (test 322)**:

```raku
my %h is BagHash;
%h<foo> = 10000000000000000000;        # 10^19, > i64::MAX (~9.2e18)
is %h<foo>, 10000000000000000000, 'can successfully set >64-bit value';
```

Same root cause: `bag.t` test 215 ("value can be larger than a native int").

## Why it fails today

- mutsu already represents `10^19` as `Value::BigInt(Arc<num_bigint::BigInt>)`
  (`.WHAT` is `Int`, prints correctly; helper `Value::bigint(n)`, `to_bigint()` at
  `src/value/mod.rs:3922`).
- But `BagData.counts` (`src/value/mod.rs:228`) is `HashMap<String, i64>` and
  `Deref`/`DerefMut` to `HashMap<String, i64>`.
- So (a) `bag_assignment_count` (`src/vm/vm_var_assign_ops.rs:839`) has **no
  `Value::BigInt` arm** → falls to the truthy→`1` fallback, and (b) `10^19`
  can't fit in `i64` anyway.

## The change

Change `BagData.counts` to `HashMap<String, num_bigint::BigInt>` (Deref target
too). ~70 sites across 18 files; most cascade automatically through `Deref`.
**MixData is NOT affected** (`weights: HashMap<String, f64>`, immutable).
**No unsafe code breaks** — `arc_contents_mut` is used on Seq/array, not Bag;
no `Arc::as_ptr`/`*mut` casts target `BagData`. Layout stays compatible.

### Useful helpers (exist / to add)
- `Value::bigint(NumBigInt) -> Value`, `Value::BigInt(Arc<NumBigInt>)`.
- `n.to_f64()`, `n.to_i128()`, `n.to_usize()` from `num_traits::ToPrimitive`
  (already imported for BigInt elsewhere).
- ADD saturating converters (suggest `src/runtime/utils.rs`):
  ```rust
  fn bigint_to_f64_sat(n: &BigInt) -> f64 { n.to_f64().unwrap_or(if n.sign()==Plus {f64::INFINITY} else {f64::NEG_INFINITY}) }
  fn bigint_to_i128_sat(n: &BigInt) -> i128 { n.to_i128().unwrap_or(if n.sign()==Plus {i128::MAX} else {i128::MIN}) }
  ```

## Migration order (lowest-risk first)

1. **Struct + constructors** (`src/value/mod.rs`): `BagData.counts`, `BagData::new`,
   `with_original_keys`, `Deref`/`DerefMut` target, and the `Value::bag` /
   `bag_hash` / `bag_typed` / `bag_hash_typed` constructors (~line 3262). Plus
   `SerValue::Bag` (`src/value/serde_support.rs:40,156,340`).
2. **Coercion** — add the BigInt arm:
   - `bag_assignment_count` (`vm_var_assign_ops.rs:839`) → return BigInt;
     `Value::BigInt(n) => Ok((**n).clone())`, `Value::Int(i) => Ok(i.into())`.
   - `quanthash_coerce.rs` count aggregation (lines ~243,256,267,301,317,346):
     `*counts.entry(k).or_insert(0i64) += weight` → BigInt entries.
   - `vm_control_ops.rs:2122/2130` (the `quanthash_set_weight` Bag arm — already
     pub(crate)): `count <= 0` removes; insert BigInt.
   - `methods_dispatch_new.rs:248`, `vm_call_method_mut_ops.rs:1008`.
3. **Set-op / arithmetic helpers** — DECISION: keep these **i64 (saturating)**;
   union/intersection/diff min/max semantics don't need arbitrary precision and
   the callers build temporary i64 maps. Saturate when reading a BigInt count.
   - `src/runtime/utils.rs`: `to_bag_map`, `coerce_to_bag`, `resolve_bag_tab_keys`,
     `set_diff/intersect/sym_diff_values` (~2603-2892).
   - `src/vm/vm_set_ops.rs`: `exec_set_union_op` (max), `exec_set_addition_op`
     (`*e += v` → `*e = e.clone()+v`), `bag_insert_item` (~498-547).
   - `src/runtime/ops.rs`: `multiply_bag_counts` (~191-250, returns `(i64,bool)`).
4. **Sampling (HIGH-RISK)** — convert weight→i128/f64 with saturation:
   - `methods_dispatch_match3.rs:406-459` `dispatch_bag_grab` (sum→f64*rand→i64;
     decrement `*c -= 1` → `*c -= BigInt::one()`; `*c <= 0` removes).
   - `methods_narg.rs:481-508` `sample_weighted_bag_key`, `1757-1803` pick,
     `1967-1978` pickpairs (return `Value::bigint(count)` not `Value::Int`).
   - `methods_mut.rs:2901/2934` grab (`sum::<i64>()` → BigInt sum → `to_usize` sat).
5. **Introspection / display**:
   - `.total` (`methods_0arg/collection.rs:812`): `sum::<i64>()` → BigInt sum →
     **return `Value::bigint`** (a Bag total can exceed i64::MAX).
   - Bag display/gist/raku (`src/value/display.rs`): count→string, likely
     automatic via Deref but verify the weight is printed as the BigInt.

## HIGHEST-RISK (verify by hand + test with 10^19)

1. `bag_assignment_count` — the actual bug; add BigInt arm (else still →1).
2. `dispatch_bag_grab` — `sum() as f64` precision/overflow; saturate.
3. `sample_weighted_bag_key` / pick — i128 sums; saturate.
4. `.total` — must return BigInt, not truncate.
5. `exec_set_addition_op` — no in-place `+=` on BigInt without clone.

## Validation
- `t/` new test: `my %h is BagHash; %h<foo> = 10**19; is %h<foo>, 10**19;` plus
  `.total`, `(+)` union of big-weight bags, round-trip serialize.
- `MUTSU_FUDGE=1 prove -e target/debug/mutsu roast/S02-types/baghash.t` → expect
  **344/344**, then add to `roast-whitelist.txt` (keep `LC_ALL=C sort -c`).
- Also re-check `roast/S02-types/bag.t` (test 215 should flip) and `mix*.t`,
  `set*.t`, `setbagmix` for regressions; full `make test` + CI `make roast`.

## Context
- Campaign history + all merged PRs: memory `[[sigilless-rw-and-quanthash-writeback]]`.
- Unsafe-cast caution: memory `[[hashdata-layout-unsafe-cast-audit]]` (audit holds;
  BagData is not currently cast unsafely, but re-grep before relying on it).
