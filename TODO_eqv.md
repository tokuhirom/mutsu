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

### Phase 4: Fix is-deeply (Partially resolved)
- Fixed `Range.Seq` to expand range elements instead of wrapping as a single element.
- Attempted switching `is-deeply` from `==` (PartialEq) to `.eqv()`, but reverted because
  `decontainerize()` converts `ArrayKind::Array → ArrayKind::List` at call sites. This means
  `@`-sigiled arguments lose their container type when passed to `is-deeply`, causing false
  negatives (e.g., `is-deeply @a, [1,2,3]` fails because `@a` arrives as List).
- `is-deeply` remains on PartialEq until decontainerize is fixed (see Phase 5).

### Phase 5: Fix decontainerize to preserve ArrayKind
`decontainerize()` currently converts ALL ArrayKinds to `List`. It should only strip
itemization: `ItemArray → Array`, `ItemList → List`, leaving `Array` and `List` unchanged.

However, many places in the codebase use `!kind.is_real_array()` (i.e., `List | ItemList`) as
the condition for flattening/expanding arrays. Changing `decontainerize` requires also updating
these sites to use `kind.is_itemized()` instead. This is a large refactor affecting:
- `builtins/functions.rs`
- `builtins/methods_narg.rs`, `builtins/methods_0arg/mod.rs`
- `vm/vm_misc_ops.rs`, `vm/vm_var_ops.rs`
- `runtime/ops.rs`, `runtime/methods.rs`, `runtime/methods_mut.rs`, `runtime/types.rs`

Once decontainerize is fixed, `is-deeply` can switch to `.eqv()`.

### Phase 6: Add diagnostics to is-deeply failure output
When `is-deeply` fails, print `expected`/`got` diagnostics using `.raku` representation,
matching Raku's output format:
```
# expected: $(1, 2, 3)
#      got: $[1, 2, 3]
```
