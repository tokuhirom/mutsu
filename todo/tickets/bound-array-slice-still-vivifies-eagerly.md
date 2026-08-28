# Slice and multi-dim subscripts still vivify array elements eagerly

## Symptom

The single-index `:=` bind stopped vivifying eagerly (`Value::array_slot_ref`
hands out a deferred `HashEntryRef` token rooted on the array for an index past
the end), but the **slice** form did not:

```raku
my @a = 1, 2;
my @s := @a[1,5];
say @a.raku;   # raku: [1, 2]     mutsu: [1, 2, Any, Any, Any, Any]
@s = 8, 9;
say @a.raku;   # raku: [1, 8, Any, Any, Any, 9]   mutsu: same (already correct)
```

Only the bind-time growth diverges; the write-through, the aliasing in both
directions (`@a[5] = 99` visible through `@s[1]`, and `@s[1] = 77` visible
through `@a`) and the fixed-arity RHS truncation all already match raku.

## Root cause

Three call sites deliberately call `Value::array_grow_to(idx)` *before*
`array_slot_ref(idx, true)` to keep the new deferred token out of a place that
cannot carry one. Each is marked with a `// TODO: hand out the deferred array
token here too`:

- `exec_index_autovivify_lazy_op`'s slice arm (`src/vm/vm_var_index_ops.rs`,
  the `slice_bind_indices` branch) — the bound slice above;
- `multi_dim_scalar_autoviv_cell` (`src/vm/vm_var_multidim_ops.rs`) — the
  all-scalar `@a[0;0;3]` descent;
- `collect_multi_dim_leaf_cells` (same file) — the slice-dimension
  `@a[*;0;3]` descent.

In all three the promoted cell ends up as an **element of another array** (the
slice list the op pushes, or the local `@s`), and the array chokepoints do not
know about tokens:

- `Interpreter::resolve_array_entry` (`src/vm/vm_var_ops.rs`) has a
  `ValueView::ContainerRef` arm but no `HashEntryRef` arm, so the element reads
  back as the raw token — this is what broke
  `roast/S32-array/multislice-6e.t`'s `@array[0;0;3] gives Any` rows when the
  primitive was made lazy without a compensating grow;
- the `.raku`/`.gist` element formatting walks the storage directly, so the
  token would print as itself;
- the bound-slice write-through (`vm_var_assign_local.rs` and
  `vm_var_assign_set_local.rs`, both gated on
  `items.iter().any(Value::is_container_ref)`) only writes through
  `ValueView::ContainerRef` elements, so a token element would silently swallow
  its RHS value — `@array[*;0;3] = 999` in the same roast file — and a slice
  that is entirely out of range (`@a[5,6]`) would not even pass the gate.

## Why it is a separate slice

Making an array *element* legitimately hold a deferred token means teaching the
read chokepoint, the display path and both write-through sites about it, plus
deciding whether the write should install a `ContainerRef` cell at the terminal
(to keep the bidirectional aliasing the slice has today) or just insert the
plain value. That is a coherent unit of work but a different one from the
single-index primitive, and getting it wrong regresses aliasing and assignment
that currently match raku exactly.

## Reproduce

The snippet above, no fixtures. `t/array-slot-ref-deferred-vivification.t`
covers the single-index form that is already fixed; a slice row belongs here
once this lands. `roast/S32-array/multislice-6e.t` is the regression oracle for
the two multi-dim sites.
