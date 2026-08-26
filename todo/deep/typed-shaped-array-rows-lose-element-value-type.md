# Typed shaped multidim array rows lose their element `value_type`, so `hole_at`'s typed-gap-marker check never fires below the outer dimension

Found while fixing `todo/tickets/multidim-exists-adverb-blind-to-initialized-and-typed-holes.md`
(see `news/2026-08/multidim-exists-adverb-canonical-hole-predicate.md` for what that ticket actually
fixed). That fix made `ArrayData::hole_at`'s typed-element-marker recognition reachable from every
multidim (`;`-separated) `:exists`/`:kv`/`:p`/`:delete` adverb call site, and it works correctly for a
1D typed array reached through an *outer* untyped dimension:

```
my Int @a[3]; @a[0] = 1;
my @outer; @outer[0] = @a;
say @outer[0;*]:exists;   # (True False False) -- correct, matches raku's own @a[0]:exists per-index
```

But it still fails for a genuinely **2+-dimensional typed shaped array** — the case the ticket's own
fix sketch used as its example:

```
my Int @a[2;2]; @a[0;0] = 1;
say @a[0].WHAT;      # mutsu: (Array)         -- should carry the element type
say @a[0;1]:exists;  # mutsu: True            -- should be False (an unwritten Int slot is a hole)
```

There is no direct raku oracle for `@a[0].WHAT` or `@a[0;1]:exists` here: real Rakudo throws
`Partially dimensioned views of shaped arrays not yet implemented. Sorry.` for `@a[0]` on a *shaped*
array, and separately `:exists on multi-dimensional slices not yet implemented. Sorry.` for
`@a[0;1]:exists` when any dimension involves a slice/Whatever context (see the parent ticket/news entry
for the fuller survey of what raku does and does not support here). So this finding is internal
self-consistency evidence, not a raku-comparable regression: mutsu's own single-dimension `:exists`
(`my Int @a[3]; @a[0]:exists`) is raku-verified correct, and the fully-materialized construction of a
1D typed array (`Array[Int].new(...)`, or a `my Int @a[3]` declaration) sets `ArrayData::value_type =
Some("Int")` on that array so `hole_at`'s `Package(name)` arm recognizes an unwritten `Int` marker as a
gap. A 2D+ *shaped* typed array's rows do not carry that.

## Root cause

`make_shaped_array_seeded` (`src/runtime/methods_signature_shaped.rs:107-152`) builds a shaped array
recursively, one dimension at a time:

```rust
pub(super) fn make_shaped_array_seeded(dims: &[usize], seed: &Value) -> Result<Value, RuntimeError> {
    ...
    if dims.len() == 1 {
        let mut items = Vec::new();
        Self::autoviv_resize(&mut items, len, seed.clone())?;
        let value = Value::shaped_array_unassigned(items);
        crate::runtime::utils::mark_shaped_array(&value, Some(dims));
        return Ok(value);
    }
    let mut items = Vec::new();
    for _ in 0..len {
        let child = Self::make_shaped_array_seeded(&dims[1..], seed)?;
        crate::runtime::utils::mark_shaped_array(&child, Some(&dims[1..]));
        items.push(child);
    }
    let value = Value::shaped_array(items);
    crate::runtime::utils::mark_shaped_array(&value, Some(dims));
    Ok(value)
}
```

Each recursive call correctly seeds the leaf row's element type object (`seed`, e.g.
`Value::package("Int")`) into every cell via `autoviv_resize`, and correctly marks the row's `shape` via
`mark_shaped_array`/`mark_shaped_array_items` (`src/runtime/utils/shaped.rs`). But nothing here ever
sets `ArrayData::value_type` on the leaf row's own `ArrayData` (or on any intermediate row). Only
whatever top-level construction path builds a *1D* typed array (`my Int @a[3]`, `Array[Int].new(...)`)
sets `value_type`; that path is never invoked for the nested rows a 2D+ shaped declaration builds
directly. `hole_at`'s `Package(name)` arm needs exactly this:

```rust
Some(ValueView::Package(name)) => {
    let is_gap_marker = name == "Any" || self.value_type.as_deref().is_some_and(|t| name == t);
    is_gap_marker && self.initialized.as_ref().is_some_and(|s| !s.contains(&i))
}
```

With `value_type == None` on the row, `is_gap_marker` is only true for the untyped `"Any"` marker, never
for `"Int"` — so an unwritten `Int` slot in a 2D+ typed shaped array is never recognized as a hole,
regardless of how correct the *reader* (`hole_at`, or any of its callers) is.

## Why this is a separate, deeper issue than the ticket it was found under

The parent ticket's fix threads a hole flag through `multidim_collect_leaves` and consolidates every
open-coded `:exists`-family predicate in `builtins_multidim_ops.rs` onto `ArrayData::hole_at` (plus a
write-side `initialized`-tracking fix for multidim element assignment, `src/vm/vm_var_multidim_ops.rs`
and `src/vm/vm_var_multidim_helpers.rs` — see the news entry). All of that is scoped to *how the
existing `hole_at` data is consumed and kept accurate on write*. This finding is about a *third* input
`hole_at` needs (`value_type`) never being populated at all for shaped-array rows below the top level —
a construction-time gap in `methods_signature_shaped.rs`, unrelated to the predicate-consolidation or
`initialized`-tracking work. Fixing it requires either:

- Threading the declared element type down through `make_shaped_array_seeded`'s recursion and setting
  `ArrayData::value_type` on every row (not just seeding cell values with the type object), or
- Deciding value_type propagation is not needed below the outer dimension and instead special-casing
  `hole_at`'s `Package` check to also match *any* boxed non-"Any" `Package` name when the array has no
  `value_type` recorded but its Whatever/multidim ancestor did — a weaker, less precise rule that risks
  false positives (a legitimately-stored `Package(SomeOtherClass)` value would misread as a hole).

Given raku itself does not support the constructs needed to observe this directly (`@a[0].WHAT` on a
shaped array, `:exists` on a multi-dimensional Whatever/slice), there is no reference behavior to match
against — only mutsu's own self-consistency (single-dim typed `:exists` is correct; nested-row typed
`:exists` is not) to preserve. This makes it lower urgency than a raku-divergence bug, but still worth
fixing for internal consistency once someone revisits shaped-array construction.

## Minimal repro

```
my Int @a[2;2]; @a[0;0] = 1;
say @a[0].WHAT;      # (Array)  -- expected (Array[Int]) or equivalent value_type tracking
say @a[0;1]:exists;  # True     -- expected False (unwritten Int slot)
```
