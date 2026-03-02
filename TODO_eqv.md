# TODO: @-variable Array/List consistency for eqv

## Background

The `eqv` operator was fixed to correctly distinguish Array from List per the Raku spec
(raku-doc: Language/operators.rakudoc "infix eqv"):

```
1 eqv 1.0      # False (Int vs Num)
[1,2] eqv (1,2) # False (Array vs List)
```

## Resolved (PR #696: ArrayKind enum)

PR #696 replaced `Value::Array(Arc<Vec<Value>>, bool)` with `Value::Array(Arc<Vec<Value>>, ArrayKind)`
where `ArrayKind` has 4 variants: `List`, `Array`, `ItemList`, `ItemArray`.

This resolved:
- **Phase 1 (Scalar container)**: Implemented via `ArrayKind::ItemList` / `ArrayKind::ItemArray`
  instead of a separate `Value::Scalar` variant. `.item` method wraps via `kind.itemize()`.
- **Phase 3 (.raku output)**: Array=`[...]`, List=`(...)`, ItemArray=`$[...]`, ItemList=`$(...)`.
- **Roast test regressions**: All 4 tests that were temporarily removed from the whitelist
  now pass again and are back in the whitelist:
  - `roast/S02-literals/listquote.t`
  - `roast/S06-currying/assuming-and-mmd.t`
  - `roast/S06-currying/misc.t`
  - `roast/S06-currying/slurpy.t`

## Resolved: Phase 2 (coerce_to_array)
After PR #696, `coerce_to_array()` already converts ALL inputs to `ArrayKind::Array`.
Verified: `my @a = 1,2,3; say @a eqv [1,2,3]` → `True`, `@a.WHAT` → `(Array)`.

## Resolved: Phase 4 + Phase 5 (decontainerize + is-deeply eqv)

Fixed `decontainerize()` to only strip itemization (`ItemArray→Array`, `ItemList→List`)
instead of converting everything to `List`. Updated flatten detection across the codebase
from `!kind.is_real_array()` to `!kind.is_itemized()` for call-site flattening (comma
operator, slurpy params, reduction, etc.). For `.flat` method specifically, only
`ArrayKind::List` is flattened (matching Raku semantics where `.flat` on Array is a no-op).

Switched `is-deeply` from `PartialEq` to `.eqv()` semantics per Raku spec.

Also fixed `eqv` for `BigInt` values (was missing match arm, falling through to `false`).

### Roast tests removed from whitelist (regression from eqv strictness)

These tests use `is-deeply` with type mismatches that were hidden by PartialEq.
Most fail because operations return List but tests compare with `[...]` (Array),
or because of allomorph (IntStr) handling gaps.

1. `roast/S02-lists/tree.t`
2. `roast/S02-literals/adverbs.t`
3. `roast/S02-magicals/args.t`
4. `roast/S02-types/range-iterator.t`
5. `roast/S03-operators/repeat.t`
6. `roast/S03-operators/subscript-adverbs.t`
7. `roast/S03-operators/u2212-minus.t`
8. `roast/S12-class/attributes-required.t`
9. `roast/S12-methods/parallel-dispatch.t`
10. `roast/S32-array/create.t`
11. `roast/S32-container/roundrobin.t`
12. `roast/S32-hash/iterator.t`
13. `roast/S32-hash/push.t`
14. `roast/S32-list/cross.t`
15. `roast/S32-list/grep.t`
16. `roast/S32-list/produce.t`
17. `roast/S32-list/squish.t`
18. `roast/S32-str/sprintf-c.t`
19. `roast/S32-str/sprintf-u.t`

Also removed `roast/S17-supply/map.t` (pre-existing failure, unrelated to this change).

### Root causes to fix for re-whitelisting

- **Hyper dispatch** (`».?`, `».*`, `».+`) returns List instead of Array
- **Many methods** (`.list`, `.grep`, `.map`, `.squish`, `.produce`, etc.) return List
  where Raku returns Array or Seq — need per-method audit of return container types
- **Allomorph** (IntStr/NumStr/RatStr) not fully implemented — `val()` returns plain Int
- **BigInt .raku** output shows trailing `.0` (e.g., `244140625000000000000.0` instead of
  `244140625000000000000`)

## Remaining work

### Phase 6: Add diagnostics to is-deeply failure output
When `is-deeply` fails, print `expected`/`got` diagnostics using `.raku` representation,
matching Raku's output format:
```
# expected: $(1, 2, 3)
#      got: $[1, 2, 3]
```
