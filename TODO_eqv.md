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

### Phase 2: Fix coerce_to_array
`coerce_to_array()` still produces `ArrayKind::List` for non-array values. Ideally `my @a = 1,2,3`
should produce `ArrayKind::Array`. This hasn't caused test failures yet but is technically incorrect
per Raku semantics.

### Phase 4: Fix is-deeply
Switch `is-deeply` from `==` (PartialEq) to `eqv` (structural equivalence).
Handle `Seq:D` → List conversion per raku-doc (Type/Test.rakudoc).
