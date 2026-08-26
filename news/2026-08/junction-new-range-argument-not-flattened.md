# `Junction.new(TYPE, values)` flattens any iterable values argument

From the doc-diff harness (`Type/Junction.rakudoc:266`):

```raku
say Junction.new("one", 1..6).Bool;   # raku: False   mutsu: True
```

## Root cause

`methods_object_dispatch_new.rs`'s `"Junction"` arm built the eigenstate list by
matching exactly three views:

```rust
ValueView::Array(items, ..) => items.to_vec(),
ValueView::Seq(items)       => items.to_vec(),
ValueView::Slip(items)      => items.to_vec(),
_                           => vec![v.clone()],
```

Anything else — a `Range`, a `Hash`, a `Set`/`Bag`/`Mix` — fell into the
single-element arm. `Junction.new("one", 1..6)` was therefore a `one()` junction
over ONE (truthy) `Range`, which trivially satisfies `one`, instead of a `one()`
over six truthy integers, which does not.

Rakudo binds `\values` and stores `values.list`, so the rule is uniform: every
iterable flattens, a `Str`/`Int` does not. Measured:

| argument | raku eigenstates |
| --- | --- |
| `1..6` | `1, 2, 3, 4, 5, 6` |
| `(1,2)` / `[1,2]` / `$(1,2)` / `(1..3).Seq` | the elements |
| `{a=>1,b=>2}` | `:a(1), :b(2)` |
| `set(1,2)` | `1 => True, 2 => True` |
| `"abc"` / `5` | one eigenstate |

## Fix

The arm now calls `value_to_list_for_receiver`, which is exactly `.list` on the
argument itself (it ignores the argument's own itemization, as `\values` does).
All eight measured shapes above now agree with `raku`, including
`Junction.new("one", 1..6).Bool` → `False`.

Pinned by `t/lazy-gather-and-junction.t` (order-free assertions, since a Hash's
pair order is not promised).
