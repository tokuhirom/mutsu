# `:exists` on a shaped array reports its holes

Being fixed-size says how many slots a shaped array has, not that anything was
written to them. raku distinguishes the two; mutsu answered `True` for every
in-range index:

```raku
my @t[3];
say @t[0]:exists;      # was True,  now False (raku: False)
say @t.EXISTS-POS(1);  # was True,  now False (raku: False)
@t[1] = 5;
say @t[1]:exists;      # True everywhere
say @t[0]:exists;      # was True,  now False
```

All three single-index `:exists` sites carried an explicit shaped carve-out —
`slot_present_at` in `exec_exists_index_adv_op`, `native_method_1arg`'s
`EXISTS-POS`, and the value-level `EXISTS-POS` in `methods_call_dispatch.rs` —
each returning "in range therefore present" before consulting the hole
predicate, and each added to keep the three consistent with one another. (The
multidimensional path never had one and was already right.)

The predicate mutsu already had was the correct one: `ArrayData::hole_at` reads
the embedded `initialized` set, which is exactly "was this slot explicitly
assigned". What was missing was the other half — a shaped array is allocated
pre-filled with its element type object and left `initialized` at `None`, which
means "bulk-constructed, no gaps", so dropping the carve-outs alone would have
been wrong in the opposite direction. `make_shaped_array` now seeds an *empty*
`initialized` set (`Value::shaped_array_unassigned`), which says "every gap
marker here is a hole", and all three sites defer to `hole_at`.

Two things had to hold for that seed to survive:

- **The typed re-seed must keep it.** `my Int @a[3]` re-seeds its cells with the
  element type object through `coerce_typed_array_elements`, whose callers
  rebuilt the backing with `ArrayData::new`, dropping `initialized`. They now
  rebuild with `Value::array_data_keeping_initialized`, which carries the set
  across the coercion and nothing else. The "nothing else" is load-bearing: the
  existing `Value::array_data_like` also carries `shape`, and stamping that onto
  every rebuilt *row* of a shaped array makes each row look like a shaped array
  of its own, which routes element writes through the multidimensional slow path
  — 6.6s to over 300s on the 100M-cell
  `roast/integration/deep-recursion-initing-native-array.t`. A native element
  type keeps the cheaper `None`: its unset cells become the numeric/string zero,
  a real value, so there are no gap markers to track (and no per-write set to
  maintain across 20k recursive writes).
- **A native shaped array is not made of holes.** `array[int].new(:shape(5))` is
  five real `0`s in raku, so every slot exists — but mutsu's constructor left the
  untyped `Any` marker in the cells, which the old carve-out hid and the new
  predicate would have reported as five holes. It renders wrong too:
  `.raku` printed `array[int].new(:shape(5,), [Any, Any, Any, Any, Any])` where
  raku prints five `0`s. `make_shaped_array_seeded` takes the unset-cell seed
  from the constructor's element type now, so a native one fills with the
  numeric/string zero and a boxed one (`Array[Int].new(:shape(3))`) with the type
  object, which stays a hole.

Pinned in `t/shaped-array-exists-holes.t`: untyped, initialized, multidimensional
and typed shaped arrays, `:delete` re-opening a slot, and an unshaped array as
the control. All 32 assertions pass unmodified under rakudo.

Found while routing `:delete` on a mixin through DELETE-KEY/DELETE-POS
([delete-adverb-dispatches-through-a-mixin](../2026-07/delete-adverb-dispatches-through-a-mixin.md)),
which made the hole-blind `EXISTS-POS` visible; the unshaped half was fixed
there.

## Left open

Two neighbours turned up while pinning this and are recorded as their own
tickets:

- The `:v` / `:k` adverbs use a *second*, `Any`-only hole predicate in
  `resolve_positional_scalar`, so a typed array's hole reads back as the element
  type object where `:exists` correctly calls it missing —
  `my Int @j; @j[2] = 5; say (@j[0]:v)` is `(Int)` and should be `()`. That is
  independent of shape (it is wrong for an unshaped typed array too).
- A *slice* `:exists` on a shaped array collapses to a scalar:
  `my @z[3]; (@z[0,1]:exists)` is one `Bool` where raku gives two, because an
  Array-valued index on a shaped target is read as a multidimensional index
  rather than a slice.
