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

## Remaining work

### Phase 2: coerce_to_array (Resolved)
After PR #696, `coerce_to_array()` already converts ALL inputs to `ArrayKind::Array`.
Verified: `my @a = 1,2,3; say @a eqv [1,2,3]` → `True`, `@a.WHAT` → `(Array)`.

### Phase 4: Fix is-deeply (Resolved)
Switched `is-deeply` from `==` (PartialEq) to `.eqv()` (structural equivalence).
Also fixed `Range.Seq` to expand range elements instead of wrapping the Range as a single element.
This exposed 21 whitelisted roast tests that were passing incorrectly due to PartialEq ignoring
type differences. These tests have been temporarily removed from the whitelist.

### Phase 5: Fix return types exposed by is-deeply eqv switch
The following 21 roast tests were removed from the whitelist because `is-deeply` now correctly
distinguishes types. The root causes are methods returning wrong container types (e.g., Array
instead of Seq). Fix the return types and re-add to whitelist.

- `roast/S02-lists/tree.t` — `.tree` returns Array/List, should return Seq
- `roast/S02-literals/adverbs.t` — `:a[16,42]` produces List, should produce Array
- `roast/S02-magicals/args.t`
- `roast/S02-names-vars/signature.t`
- `roast/S03-operators/repeat.t`
- `roast/S03-operators/subscript-adverbs.t`
- `roast/S03-operators/u2212-minus.t`
- `roast/S12-class/attributes-required.t`
- `roast/S12-methods/parallel-dispatch.t`
- `roast/S17-supply/Seq.t`
- `roast/S17-supply/act.t`
- `roast/S17-supply/lines.t`
- `roast/S17-supply/list.t`
- `roast/S32-array/create.t`
- `roast/S32-container/roundrobin.t`
- `roast/S32-hash/push.t`
- `roast/S32-list/cross.t`
- `roast/S32-list/produce.t`
- `roast/S32-list/squish.t` — `.squish` returns Array, should return Seq
- `roast/S32-list/unique.t` — `.unique` returns Array, should return Seq
- `roast/S32-num/power.t`

### Phase 6: Add diagnostics to is-deeply failure output
When `is-deeply` fails, print `expected`/`got` diagnostics using `.raku` representation,
matching Raku's output format:
```
# expected: $(1, 2, 3)
#      got: $[1, 2, 3]
```
