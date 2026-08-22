# `multidim_exists_adverb_multi`'s hole predicate is blind to `initialized` and typed-array gap markers

`multidim_exists_adverb_multi` (`src/runtime/builtins_multidim_ops.rs`, around line 415-419) computes
"does this leaf exist" for a multi-dimensional `:exists` query carrying a `Whatever`/list index
(`@a[*;*]:exists`, `@a[]:kv`, etc.) with its own open-coded predicate:

```rust
let raw_exists = !value.is_nil()
    && !matches!(value.view(), ValueView::Package(name) if name == "Any");
```

This is one of the "three divergent hole predicates" ADR-0049
(`docs/adr/0049-nil-decays-to-the-container-default-at-the-element-store.md`) §1.6/§4 slice 5 named for
folding onto the canonical `ArrayData::hole_at`. Two gaps remain after ADR-0049 slices 0-6 landed:

1. **It only recognizes the literal `"Any"` package name**, not a typed array's own element-type marker
   (`my Int @a[3]; @a[0]:exists` in a multidim/Whatever context would not recognize an unassigned `Int`
   gap marker as a hole).
2. **It never consults `ArrayData::initialized`**, so an explicitly-assigned `Any`/type-object value
   (which is NOT a hole) is indistinguishable from a genuine gap in this one code path.

## Why it was not folded onto `hole_at` directly

`multidim_exists_adverb_multi` gets its `(path, value)` pairs from `multidim_collect_leaves`
(`src/runtime/builtins_multidim.rs`), a general-purpose leaf collector shared by six call sites across
`builtins_multidim_ops.rs` (slice/kv/p/AT-POS-family handling for `Whatever`/list multidim indices). It
only carries the extracted leaf *value*, not the owning `ArrayData` + index that produced it, so there is
no direct way to call `.hole_at(i)` at the point this function inspects a leaf.

Properly fixing this requires either:
- Threading an extra `is_hole: bool` (or a full `(ArrayData, index)` reference) through
  `multidim_collect_leaves`'s recursion and its `Vec<(Vec<Value>, Value)>` output type, touching all six
  call sites and every recursive arm (`Whatever`-over-Array, `Whatever`-over-Hash, Array-of-indices,
  single-index fallback), or
- Splitting a parallel, `:exists`-specific leaf collector that also returns the hole flag.

## Why this was deferred rather than fixed as part of ADR-0049

- It is a narrow surface: only the `Whatever`/list-index form of `:exists` on a genuinely
  multi-dimensional target is affected. The common, non-multidim `:exists` path
  (`src/vm/vm_var_exists_ops.rs`) already routes through `ArrayData::hole_at` correctly.
- After ADR-0049 slices 1-4, `value.is_nil()` can no longer be true for a real array element reached this
  way (a real element never holds `Nil` any more), so the existing check is not actively WRONG for the
  untyped case today -- it is incomplete (misses typed markers, ignores `initialized`), not broken.
- The invasive refactor (new tuple shape across 6 call sites) carries real risk of a subtle regression in
  a complex recursive function, for a benefit limited to typed multidim arrays combined with a
  `Whatever`/list `:exists` query -- a combination with no roast coverage found during a spot-check of
  the whitelisted `S09-multidim`/`S32-array` files (all pass).

## Fix sketch

Extend `multidim_collect_leaves`'s output to `Vec<(Vec<Value>, Value, bool)>` (the third element: "was
this leaf a hole in its immediate parent array"), computed at each `Array`-level iteration site
(`items.hole_at(i)`) before recursing, and threaded through every recursive call including the
`ContainerRef`/`Scalar` deref arm. Update the five other call sites to destructure the new triple (most
can ignore the bool). Then replace `multidim_exists_adverb_multi`'s `raw_exists` computation with the
threaded flag instead of re-deriving it from the bare value.
